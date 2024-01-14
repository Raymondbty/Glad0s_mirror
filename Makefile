NAME = glados

all:
	stack build
	cp $$(stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean
	$(RM) -f *~
	$(RM) -f *.o
	$(RM) -f *.hi

fclean: clean
	$(RM) -f $(NAME)

test:
	stack test --coverage

re: fclean all

docs:
	stack haddock

.PHONY: re fclean clean test all docs
