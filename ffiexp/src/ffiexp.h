#ifndef SOCK_H
#define SOCK_H


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int mysocket ();

int clientconnect(int sockfd, struct hostent *server, int portno);

int serverconnect(int sockfd, int portno);

int mywrite(int sockfd, char *buf);

void *allocBuf(int len);

void printP(void *ptr);

void freeBuf(void *buf);

void *id(void *ptr);

int isNull(void *ptr);

#endif

