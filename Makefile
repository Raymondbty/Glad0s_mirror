BINARY_PATH	:=	$(shell stack path --local-install-root)

NAME	= 	glados

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
		stack clean
		$(RM) -f *~
		$(RM) -f *.o
		$(RM) -f *.hi

fclean:	clean
		$(RM) -f $(NAME)

test:
	stack test --coverage
	open .stack-work/install/x86_64-linux/a95b720bfbea6c75b9282490f954248b9dd53610dc431fdc67dab12c0c5a2261/9.6.3/hpc/index.html

re:	fclean all

.PHONY:	re fclean clean test all
