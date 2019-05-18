#include <GL/glut.h>

/*
 *  Para compilarlo, necesitas el paquete llamado freeglut, en OpenBSD lo puedes instalar con:
 *
 *  pkg_add -iv freeglut
 *
 *  y checas la ubicacion de la libreria: 
 *
 *  $ pkg_info -L freeglut
 *  Information for inst:freeglut-3.0.0
 *
 *  Files:
 *  /usr/local/include/GL/freeglut.h
 *  /usr/local/include/GL/freeglut_ext.h
 *  /usr/local/include/GL/freeglut_std.h
 *  /usr/local/include/GL/glut.h
 *  /usr/local/lib/libglut.a
 *  /usr/local/lib/libglut.so.6.0
 *  /usr/local/lib/pkgconfig/freeglut.pc
 *  
 *  Con esto ya puedes compilarlo:
 *
 *  $ gcc -o flor flor.c    \
 *  -I/usr/X11R6/include    \  # Ubicacion de <GL/gl.h> y de <GL/glu.h>
 *  -I/usr/local/include    \  # Ubicacion de <GL/glut.h>
 *  -L/usr/X11R6/lib        \  # Ubicacion de GL.so y de GLU.so
 *  -L/usr/local/lib        \  # Ubicacion de glut.so
 *  -lm -lGL -lGLU -lglut   \  # Las librerias que necesitas incluyendo a <math.h>
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

enum {ID_EXIT, ID_FULL_SCREEN};

typedef struct _POINT {
	float x;
	float y;
	float z;
} POINT;
 
int display_list_id = 0;

int dragging = 0;
int lighting = 0;
int antialias = 1;
int fullscreen = 0;

POINT pos = {0.0, 0.0, -3.0};
POINT ang = {0.0, 0.0, 0.0};
POINT init_ang = {0.0, 0.0, 0.0};

void print_point(const POINT*);
void set_fullscreen();
void on_exit_msg();
void init_glscene();
void on_reshape_msg();
void on_menu_item_msg(int);
void on_key_press_msg(unsigned char, int, int);
void on_func_key_press_msg(int, int, int);
void on_mouse_state_msg(int, int, int, int);
void on_mouse_move_msg(int, int);
void on_dragging_mouse_msg(int, int);
void axis(float);
void on_display_msg();
void on_idle();

int main(int argc, char *argv[])
{
	// Creacion de la ventana principal
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
	glutInitWindowSize(800, 600);
	glutCreateWindow("Flor");
 	 
	init_glscene();
	glutDisplayFunc(on_display_msg);
	glutIdleFunc(on_display_msg);
	glutReshapeFunc(on_reshape_msg);
 	 
	// Asignacion de las funciones del teclado
	glutKeyboardFunc(on_key_press_msg);
	glutSpecialFunc(on_func_key_press_msg);
 	 
	// Asignacion de las funciones del mouse
	glutMouseFunc(on_mouse_state_msg);
	glutPassiveMotionFunc(on_mouse_move_msg);
	glutMotionFunc(on_dragging_mouse_msg);
	//glutIdleFunc(on_idle);
 	 
	// Asinacion del menu a la ventana principal
	glutCreateMenu(on_menu_item_msg);
	glutAddMenuEntry("Pantalla completa\tF5", ID_FULL_SCREEN);
	glutAddMenuEntry("Salir\tAlt+X", ID_EXIT);
	glutAttachMenu(GLUT_RIGHT_BUTTON);

	// Bucle principal del programa
	glutMainLoop();

	on_exit_msg();
	return 0;
}

void print_point(const POINT* p)
{
	fprintf(stderr, "(%d, %d, %d)", p->x, p->y, p->z);
}

void set_fullscreen()
{
	if (!fullscreen) {
		glutFullScreen();
		fullscreen = 1;
	}
}

void on_exit_msg() // Handles the exit function
{
}

//sub init_glscene { // Setups the OpenGL scene
//        if ($antialias) {
//                glEnable(GL_LINE_SMOOTH);
//                glEnable(GL_POINT_SMOOTH);
//                glEnable(GL_BLEND);
//                glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//                glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE);
//                glLineWidth(1.5);
//                glPointSize(10.0);
// 
//                glShadeModel(GL_FLAT);
//                glClearColor(0.0, 0.0, 0.0, 0.0);
//                glDepthFunc(GL_LESS);
//                glEnable(GL_DEPTH_TEST);
//        } else {
//                // Enable smooth shading
//                glShadeModel(GL_SMOOTH);
// 
//                // Black background
//                glClearColor(0.0, 0.0, 0.0, 0.5);
// 
//                // Depth buffer setup
//                glClearDepth(1.0);
//               
//                // Enables depth testing
//                glEnable(GL_DEPTH_TEST);
// 
//                // The type of depth testing to do
//                glDepthFunc(GL_LEQUAL);
// 
//                // Really nice perspective calculations
//                glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
//        }
// 
//        if ($lighting) {
//                glEnable(GL_LIGHT0);
//                glEnable(GL_LIGHTING);
//                glEnable(GL_AUTO_NORMAL);
//                glEnable(GL_NORMALIZE);
//                glEnable(GL_COLOR_MATERIAL);
//        }
// 
//        //glShadeModel(GL_SMOOTH);
//        //glClearColor(0.0F, 0.0F, 0.0F, 1.0F);
//        //glClearDepth(1.0F);
//        //glEnable(GL_DEPTH_TEST);
//        //glDepthFunc(GL_LEQUAL);
//        //glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
// 
//        my @coords;
//        my $points = 17;
//        my $radius = 1.0;
//        my $angle = 360.0 / $points;
// 
//        foreach my $i (1 .. $points) {
//                my $radian = ($angle * $i) * pi / 180.0;
// 
//                push(
//                        @coords,
//                        Point->new(
//                                x => $radius * cos($radian),
//                                y => $radius * sin($radian),
//                                z => 0.0
//                        )
//                );
//        }
// 
//        $display_list_id = glGenLists(1);
// 
//        if ($display_list_id) {
//                glNewList($display_list_id, GL_COMPILE);
//                glBegin(GL_LINES);
// 
//                foreach my $i (0 .. $//coords) {
//                        foreach my $j ($i .. $//coords) {       
//                                glVertex2f($coords[$i]->x, $coords[$i]->y);
//                                glVertex2f($coords[$j]->x, $coords[$j]->y);
//                        }
//                }
// 
//                glEnd;
//                my $quadric_object = gluNewQuadric();
// 
//                if ($quadric_object) {
//                        gluQuadricDrawStyle($quadric_object, GLU_SILHOUETTE);
//                        gluPartialDisk($quadric_object, 0.0, 1.0, 64, 32, 0, 360);
//                        gluDeleteQuadric($quadric_object);
//                }
// 
//                glEndList;
//        }
//}

void init_glscene() // Setups the OpenGL scene
{ 
	int i, j;
	int points = 17;
	POINT *coords = malloc(sizeof(POINT) * points);
	float radius = 1.0;
	float angle = 360.0 / (float)points;
	float radian;

	GLUquadric* quadric_object;

	if (antialias) {
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
		// Enable smooth shading
		glShadeModel(GL_SMOOTH);

		// Black background
		glClearColor(0.0, 0.0, 0.0, 0.5);

		// Depth buffer setup
		glClearDepth(1.0);

		// Enables depth testing
		glEnable(GL_DEPTH_TEST);

		// The type of depth testing to do
		glDepthFunc(GL_LEQUAL);

		// Really nice perspective calculations
		glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	}

	if (lighting) {
		glEnable(GL_LIGHT0);
		glEnable(GL_LIGHTING);
		glEnable(GL_AUTO_NORMAL);
		glEnable(GL_NORMALIZE);
		glEnable(GL_COLOR_MATERIAL);
	}
 
	//glShadeModel(GL_SMOOTH);
	//glClearColor(0.0F, 0.0F, 0.0F, 1.0F);
	//glClearDepth(1.0F);
	//glEnable(GL_DEPTH_TEST);
	//glDepthFunc(GL_LEQUAL);
	//glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
 
	for (i = 0; i < points; i++) {
		radian = (angle * (float)(i + 1)) * M_PI / 180.0;
		coords[i].x = radius * cos(radian);
		coords[i].y = radius * sin(radian);
		coords[i].z = 0.0;
	}

	display_list_id = glGenLists(1);

	if (display_list_id) {
		glNewList(display_list_id, GL_COMPILE);
		glBegin(GL_LINES);

		for (i = 0; i < points; i++) {
			for (j = i; j < points; j++) {
				glVertex2f(coords[i].x, coords[i].y);
				glVertex2f(coords[j].x, coords[j].y);
			}
		}

		glEnd();
		quadric_object = gluNewQuadric();

		if (quadric_object) {
			gluQuadricDrawStyle(quadric_object, GLU_SILHOUETTE);
			gluPartialDisk(quadric_object, 0.0, 1.0, 64, 32, 0, 360);
			gluDeleteQuadric(quadric_object);
		}

		glEndList();
	}
}

void on_reshape_msg(int width, int height) // Reshape the OpenGL scene
{
	// Prevents a divition by zero
	if (height) {
		// Reset the current viewport
		glViewport(0, 0, width, height);

		// Select the projection matrix
		glMatrixMode(GL_PROJECTION);

		// Reset the projection matrix
		glLoadIdentity();

		// Calculate the aspect ratio of the window
		gluPerspective(45.0, (float)width / (float)height, 0.1, 100.0);

		// Select the ModelView matrix
		glMatrixMode(GL_MODELVIEW);

		// Reset the ModeView matrix
		glLoadIdentity();
	}
}

void on_menu_item_msg(int value) // Handles the menu items messages
{
	if (value == ID_EXIT) exit(1);

	if (value == ID_FULL_SCREEN) set_fullscreen();
}

void on_key_press_msg(unsigned char key, int x, int y) // Handles the key strokes
{
	//if (key == 120) {exit if (glutGetModifiers() & GLUT_ACTIVE_ALT);}
}

void on_func_key_press_msg(int key, int x, int y) // Handles the function keys strokes
{
	float delta = 0.1;

	if (key == GLUT_KEY_F5) set_fullscreen();

	if (key == GLUT_KEY_RIGHT) pos.x += delta;
	if (key == GLUT_KEY_LEFT) pos.x -= delta;

	if (key == GLUT_KEY_UP) pos.y += delta;
	if (key == GLUT_KEY_DOWN) pos.y -= delta;

	if (key == GLUT_KEY_END) pos.z += delta;
	if (key == GLUT_KEY_HOME) pos.z -= delta;
}

void on_mouse_state_msg(int button, int state, int x, int y) // Handles the mouse button messages
{
	if (button == GLUT_LEFT) {
		if (state == GLUT_UP) dragging = 0;

		if (state == GLUT_DOWN) {
			dragging = 1;
			init_ang.x = x;
			init_ang.y = y;
		}
	}
}

void on_mouse_move_msg(int x, int y) // Handles the mouse scrolling messages
{
}

void on_dragging_mouse_msg(int x, int y) // Handles the mouse dragging messages
{
	if (dragging) {
		ang.x += (x - init_ang.x);
		ang.y += (y - init_ang.y);
		init_ang.x = x;
		init_ang.y = y;
		glutPostRedisplay();
	}
}

void axis(float size)
{
	//glGetDoublev_p(GL_CURRENT_COLOR, @color);

	glBegin(GL_LINES);
	glColor3f(1.0, 0.0, 0.0);
	glVertex3f(-size, 0.0, 0.0); glVertex3f(size, 0.0, 0.0);

	glColor3f(0.0, 1.0, 0.0);
	glVertex3f(0.0, -size, 0.0); glVertex3f(0.0, size, 0.0);

	glColor3f(0.0, 0.0, 1.0);
	glVertex3f(0.0, 0.0, -size); glVertex3f(0.0, 0.0, size);
	glEnd();

	//glColor4dv_p($color[0], $color[1], $color[2], $color[3]);
}

void on_display_msg() // Main display function
{
	// Clear the buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Reset the current matrix
	glLoadIdentity();
	glTranslatef(pos.x, pos.y, pos.z);
	glRotatef(ang.x, 0.0, 1.0, 0.0);
	glRotatef(ang.y, 1.0, 0.0, 0.0);

	if (display_list_id) glCallList(display_list_id) ;

	// Exchanges the front and back buffers
	glutSwapBuffers();
}

void on_idle()
{
        ang.x++;
        ang.y++;
        glutPostRedisplay();
}
