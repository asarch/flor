use strict;
use warnings;
 
use OpenGL qw(:all);
use Math::Trig;

use Point;

use constant {
        ID_EXIT => 0,
        ID_FULL_SCREEN => 1
};
 
my $display_list_id;
 
my $dragging = 0;
my $lighting = 0;
my $antialias = 1;
my $fullscreen = 0;
 
my $pos = Point->new(x => 0, y => 0, z => -3);
my $ang = Point->new(x => 0, y => 0, z => 0);
my $init_ang = Point->new(x => 0, y => 0, z => 0);
 
sub print_point {
        my $p = shift;
 
        print STDERR "x: ".$p->x.", y: ".$p->y.", z: ".$p->z."\n";
}
 
sub set_fullscreen { # Sets the full screen window
        if (!$fullscreen) {
                glutFullScreen;
                $fullscreen = 1;
        }
}
 
sub on_exit_msg { # Handles the exit function
}
 
sub init_glscene { # Setups the OpenGL scene
        if ($antialias) {
                glEnable(GL_LINE_SMOOTH);
                glEnable(GL_POINT_SMOOTH);
                glEnable(GL_BLEND);
                glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
                glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE);
                glLineWidth(1.5);
                glPointSize(10.0);
 
                glShadeModel(GL_FLAT);
                glClearColor(0.0, 0.0, 0.0, 0.0);
                glDepthFunc(GL_LESS);
                glEnable(GL_DEPTH_TEST);
        } else {
                # Enable smooth shading
                glShadeModel(GL_SMOOTH);
 
                # Black background
                glClearColor(0.0, 0.0, 0.0, 0.5);
 
                # Depth buffer setup
                glClearDepth(1.0);
               
                # Enables depth testing
                glEnable(GL_DEPTH_TEST);
 
                # The type of depth testing to do
                glDepthFunc(GL_LEQUAL);
 
                # Really nice perspective calculations
                glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
        }
 
        if ($lighting) {
                glEnable(GL_LIGHT0);
                glEnable(GL_LIGHTING);
                glEnable(GL_AUTO_NORMAL);
                glEnable(GL_NORMALIZE);
                glEnable(GL_COLOR_MATERIAL);
        }
 
        #glShadeModel(GL_SMOOTH);
        #glClearColor(0.0F, 0.0F, 0.0F, 1.0F);
        #glClearDepth(1.0F);
        #glEnable(GL_DEPTH_TEST);
        #glDepthFunc(GL_LEQUAL);
        #glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
 
        my @coords;
        my $points = 17;
        my $radius = 1.0;
        my $angle = 360.0 / $points;
 
        foreach my $i (1 .. $points) {
                my $radian = ($angle * $i) * pi / 180.0;
 
                push(
                        @coords,
                        Point->new(
                                x => $radius * cos($radian),
                                y => $radius * sin($radian),
                                z => 0.0
                        )
                );
        }
 
        $display_list_id = glGenLists(1);
 
        if ($display_list_id) {
                glNewList($display_list_id, GL_COMPILE);
                glBegin(GL_LINES);
 
                foreach my $i (0 .. $#coords) {
                        foreach my $j ($i .. $#coords) {       
                                glVertex2f($coords[$i]->x, $coords[$i]->y);
                                glVertex2f($coords[$j]->x, $coords[$j]->y);
                        }
                }
 
                glEnd;
                my $quadric_object = gluNewQuadric();
 
                if ($quadric_object) {
                        gluQuadricDrawStyle($quadric_object, GLU_SILHOUETTE);
                        gluPartialDisk($quadric_object, 0.0, 1.0, 64, 32, 0, 360);
                        gluDeleteQuadric($quadric_object);
                }
 
                glEndList;
        }
}
 
sub on_reshape_msg { # Reshape the OpenGL scene
        my ($width, $height) = @_;
 
        # Prevent a divition by zero
        if ($height) {
                # Reset the current viewport
                glViewport(0, 0, $width, $height);
 
                # Select the projection matrix
                glMatrixMode(GL_PROJECTION);
 
                # Reset the projection matrix
                glLoadIdentity;
 
                # Calculate the aspect ratio of the window
                gluPerspective(45.0, $width / $height, 0.1, 100.0);
 
                # Select the ModelView matrix
                glMatrixMode(GL_MODELVIEW);
 
                # Reset the ModeView matrix
                glLoadIdentity;
        }
}
 
sub on_menu_item_msg { # Handles the menu items messages
        my $value = shift;
 
        exit if $value == ID_EXIT;
 
        set_fullscreen if $value == ID_FULL_SCREEN;
}
 
sub on_key_press_msg { # Handles the key strokes
        my ($key, $x, $y) = @_;
 
        #if $key == 120 {exit if (glutGetModifiers() & GLUT_ACTIVE_ALT);}
}
 
sub on_func_key_press_msg { # Handles the function keys strokes
        my ($key, $x, $y) = @_;
        my $delta = 0.1;
 
        set_fullscreen if $key == GLUT_KEY_F5;
 
        $pos->x($pos->x + $delta) if $key == GLUT_KEY_RIGHT;
        $pos->x($pos->x - $delta) if $key == GLUT_KEY_LEFT;
 
        $pos->y($pos->y + $delta) if $key == GLUT_KEY_UP;
        $pos->y($pos->y - $delta) if $key == GLUT_KEY_DOWN;
 
        $pos->z($pos->z + $delta) if $key == GLUT_KEY_END;
        $pos->z($pos->z - $delta) if $key == GLUT_KEY_HOME;
}
 
sub on_mouse_state_msg { # Handles the mouse button messages
        my ($button, $state, $x, $y) = @_;
 
        if ($button == GLUT_LEFT) {
                $dragging = 0 if ($state == GLUT_UP);
 
                if ($state == GLUT_DOWN) {
                        $dragging = 1;
                        $init_ang->x($x);
                        $init_ang->y($y);
                }
        }
}
 
sub on_mouse_move_msg { # Handles the mouse scrolling messages
        my ($x, $y) = @_;
}
 
sub on_dragging_mouse_msg { # Handles the mouse dragging messages
        my ($x, $y) = @_;
 
        if ($dragging) {
                $ang->x($ang->x + ($x - $init_ang->x));
                $ang->y($ang->y + ($y - $init_ang->y));
                $init_ang->x($x);
                $init_ang->y($y);
                glutPostRedisplay;
        }
}
 
sub axis {
        my $size = shift;
        my @color;
 
        #glGetDoublev_p(GL_CURRENT_COLOR, @color);
 
        glBegin(GL_LINES);
        glColor3f(1.0, 0.0, 0.0);
        glVertex3f(-$size, 0.0, 0.0); glVertex3f($size, 0.0, 0.0);
       
        glColor3f(0.0, 1.0, 0.0);
        glVertex3f(0.0, -$size, 0.0); glVertex3f(0.0, $size, 0.0);
 
        glColor3f(0.0, 0.0, 1.0);
        glVertex3f(0.0, 0.0, -$size); glVertex3f(0.0, 0.0, $size);
        glEnd();
 
        #glColor4dv_p($color[0], $color[1], $color[2], $color[3]);
}
 
sub on_display_msg { # Main display function
        # Clear the buffers
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
       
        # Reset the current matrix
        glLoadIdentity;
        glTranslatef($pos->x, $pos->y, $pos->z);
        glRotatef($ang->x, 0.0, 1.0, 0.0);
        glRotatef($ang->y, 1.0, 0.0, 0.0);
 
        glCallList($display_list_id) if $display_list_id;
 
        # Exchanges the front and back buffers
        glutSwapBuffers;
}
 
sub on_idle {
        $ang->x($ang->x + 1);
        $ang->y($ang->y + 1);
        glutPostRedisplay;
}
 
# Creacion de la ventana principal
glutInit(@ARGV);
glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
glutInitWindowSize(800, 600);
glutCreateWindow("GLUT Template File");
 
init_glscene;
END { on_exit_msg }
glutDisplayFunc(\&on_display_msg);
glutIdleFunc(\&on_display_msg);
glutReshapeFunc(\&on_reshape_msg);
 
# Asignacion de las funciones del teclado
glutKeyboardFunc(\&on_key_press_msg);
glutSpecialFunc(\&on_func_key_press_msg);
 
# Asignacion de las funciones del mouse
glutMouseFunc(\&on_mouse_state_msg);
glutPassiveMotionFunc(\&on_mouse_move_msg);
glutMotionFunc(\&on_dragging_mouse_msg);
glutIdleFunc(\&on_idle);
 
# Asinacion del menu a la ventana principal
glutCreateMenu(\&on_menu_item_msg);
glutAddMenuEntry("Pantalla completa\tF5", ID_FULL_SCREEN);
glutAddMenuEntry("Salir\tAlt+X", ID_EXIT);
glutAttachMenu(GLUT_RIGHT_BUTTON);

# Bucle principal del programa
glutMainLoop;
