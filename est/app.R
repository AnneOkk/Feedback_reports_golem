
Shiny from
Get Started Gallery Articles App Stories Reference Deploy Help Contribute
Event handler

observeEvent(eventExpr, handlerExpr, event.env = parent.frame(),
             event.quoted = FALSE, handler.env = parent.frame(),
             handler.quoted = FALSE, label = NULL, suspended = FALSE, priority = 0,
             domain = getDefaultReactiveDomain(), autoDestroy = TRUE,
             ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE)

eventReactive(eventExpr, valueExpr, event.env = parent.frame(),
              event.quoted = FALSE, value.env = parent.frame(), value.quoted = FALSE,
              label = NULL, domain = getDefaultReactiveDomain(), ignoreNULL = TRUE,
              ignoreInit = FALSE)

Arguments
eventExpr 	A (quoted or unquoted) expression that represents the event; this can be a simple reactive value like input$click, a call to a reactive expression like dataset(), or even a complex expression inside curly braces
handlerExpr 	The expression to call whenever eventExpr is invalidated. This should be a side-effect-producing action (the return value will be ignored). It will be executed within an isolate scope.
event.env 	The parent environment for eventExpr. By default, this is the calling environment.
event.quoted 	Is the eventExpr expression quoted? By default, this is FALSE. This is useful when you want to use an expression that is stored in a variable; to do so, it must be quoted with quote().
handler.env 	The parent environment for handlerExpr. By default, this is the calling environment.
handler.quoted 	Is the handlerExpr expression quoted? By default, this is FALSE. This is useful when you want to use an expression that is stored in a variable; to do so, it must be quoted with quote().
label 	A label for the observer or reactive, useful for debugging.
suspended 	If TRUE, start the observer in a suspended state. If FALSE (the default), start in a non-suspended state.
priority 	An integer or numeric that controls the priority with which this observer should be executed. An observer with a given priority level will always execute sooner than all observers with a lower priority level. Positive, negative, and zero values are allowed.
domain 	See domains.
autoDestroy 	If TRUE (the default), the observer will be automatically destroyed when its domain (if any) ends.
ignoreNULL 	Whether the action should be triggered (or value calculated, in the case of eventReactive) when the input is NULL. See Details.
ignoreInit 	If TRUE, then, when this observeEvent is first created/initialized, ignore the handlerExpr (the second argument), whether it is otherwise supposed to run or not. The default is FALSE. See Details.
once 	Whether this observeEvent should be immediately destroyed after the first time that the code in handlerExpr is run. This pattern is useful when you want to subscribe to a event that should only happen once.
valueExpr 	The expression that produces the return value of the eventReactive. It will be executed within an isolate scope.
value.env 	The parent environment for valueExpr. By default, this is the calling environment.
value.quoted 	Is the valueExpr expression quoted? By default, this is FALSE. This is useful when you want to use an expression that is stored in a variable; to do so, it must be quoted with quote().
Value

observeEvent returns an observer reference class object (see observe). eventReactive returns a reactive expression object (see reactive).

Description

Respond to "event-like" reactive inputs, values, and expressions.
Details

Shiny's reactive programming framework is primarily designed for calculated values (reactive expressions) and side-effect-causing actions (observers) that respond to any of their inputs changing. That's often what is desired in Shiny apps, but not always: sometimes you want to wait for a specific action to be taken from the user, like clicking an actionButton, before calculating an expression or taking an action. A reactive value or expression that is used to trigger other calculations in this way is called an event.

These situations demand a more imperative, "event handling" style of programming that is possible--but not particularly intuitive--using the reactive programming primitives observe and isolate. observeEvent and eventReactive provide straightforward APIs for event handling that wrap observe and isolate.

Use observeEvent whenever you want to perform an action in response to an event. (Note that "recalculate a value" does not generally count as performing an action--see eventReactive for that.) The first argument is the event you want to respond to, and the second argument is a function that should be called whenever the event occurs.

Use eventReactive to create a calculated value that only updates in response to an event. This is just like a normal reactive expression except it ignores all the usual invalidations that come from its reactive dependencies; it only invalidates in response to the given event.
ignoreNULL and ignoreInit

Both observeEvent and eventReactive take an ignoreNULL parameter that affects behavior when the eventExpr evaluates to NULL (or in the special case of an actionButton, 0). In these cases, if ignoreNULL is TRUE, then an observeEvent will not execute and an eventReactive will raise a silent validation error. This is useful behavior if you don't want to do the action or calculation when your app first starts, but wait for the user to initiate the action first (like a "Submit" button); whereas ignoreNULL=FALSE is desirable if you want to initially perform the action/calculation and just let the user re-initiate it (like a "Recalculate" button).

Unlike what happens for ignoreNULL, only observeEvent takes in an ignoreInit argument. By default, observeEvent will run right when it is created (except if, at that moment, eventExpr evaluates to NULL and ignoreNULL is TRUE). But when responding to a click of an action button, it may often be useful to set ignoreInit to TRUE. For example, if you're setting up an observeEvent for a dynamically created button, then ignoreInit = TRUE will guarantee that the action (in handlerExpr) will only be triggered when the button is actually clicked, instead of also being triggered when it is created/initialized.

Even though ignoreNULL and ignoreInit can be used for similar purposes they are independent from one another. Here's the result of combining these:

ignoreNULL = TRUE and ignoreInit = FALSE
    This is the default. This combination means that handlerExpr will run every time that eventExpr is not NULL. If, at the time of the observeEvent's creation, handleExpr happens to not be NULL, then the code runs. 

ignoreNULL = FALSE and ignoreInit = FALSE
This combination means that handlerExpr will run every time no matter what. 

ignoreNULL = FALSE and ignoreInit = TRUE
This combination means that handlerExpr will not run when the observeEvent is created (because ignoreInit = TRUE), but it will run every other time. 

ignoreNULL = TRUE and ignoreInit = TRUE
This combination means that handlerExpr will not run when the observeEvent is created (because ignoreInit = TRUE). After that, handlerExpr will run every time that eventExpr is not NULL. 

Examples

## Only run this example in interactive R sessions
if (interactive()) {
    
    ## App 1: Sample usage
    shinyApp(
        ui = fluidPage(
            column(4,
                   numericInput("x", "Value", 5),
                   br(),
                   actionButton("button", "Show")
            ),
            column(8, tableOutput("table"))
        ),
        server = function(input, output) {
            # Take an action every time button is pressed;
            # here, we just print a message to the console
            observeEvent(input$button, {
                cat("Showing", input$x, "rows\n")
            })
            # Take a reactive dependency on input$button, but
            # not on any of the stuff inside the function
            df <- eventReactive(input$button, {
                head(cars, input$x)
            })
            output$table <- renderTable({
                df()
            })
        }
    )
    
    ## App 2: Using `once`
    shinyApp(
        ui = basicPage( actionButton("go", "Go")),
        server = function(input, output, session) {
            observeEvent(input$go, {
                print(paste("This will only be printed once; all",
                            "subsequent button clicks won't do anything"))
            }, once = TRUE)
        }
    )
    
    ## App 3: Using `ignoreInit` and `once`
    shinyApp(
        ui = basicPage(actionButton("go", "Go")),
        server = function(input, output, session) {
            observeEvent(input$go, {
                insertUI("#go", "afterEnd",
                         actionButton("dynamic", "click to remove"))
                
                # set up an observer that depends on the dynamic
                # input, so that it doesn't run when the input is
                # created, and only runs once after that (since
                # the side effect is remove the input from the DOM)
                observeEvent(input$dynamic, {
                    removeUI("#dynamic")
                }, ignoreInit = TRUE, once = TRUE)
            })
        }
    )
}

See also

actionButton
© Copyright 2020 RStudio Inc.
