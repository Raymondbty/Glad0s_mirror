NAME	= 	glados

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)

clean:
		stack clean
		$(RM) -f *~
		$(RM) -f *.o
		$(RM) -f *.hi

fclean:	clean
		$(RM) -f $(NAME)

test:
	stack test --coverage

re:	fclean all

.PHONY:	re fclean clean test all
