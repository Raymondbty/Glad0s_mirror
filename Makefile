##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME	=	glados

all:	$(NAME)

$(NAME):
	stack build
	cp $(shell stack path --local-install-root)/bin/$(NAME)-exe $(NAME)

clean:
	stack clean

fclean:	clean
	$(RM) -r .stack-work
	$(RM) stack.yaml.lock
	$(RM) $(NAME)

re:	fclean all

test: all
	stack test

.PHONY: all $(NAME) clean fclean re test
