# Changelog
All notable changes about agile-service will be documented in this file.
## [1.2.0] - 2022-02-11

### Added

- State machine: it supports the linkage of child tasks to any state and the change of parent task state, so as to improve the ability of automatic flow of work items and improve collaboration efficiency.

### Fixed

- Fixed multilingual display of some input boxes.

- Fixed configuration Kanban drag column interaction issue.

- Fixed the problem that the daily workload calendar per person showed when sprinting across the year.

- Fix the problem that the version affected by the problem item is not logged.

- Fix the problem of continuously modifying epic, feature and version color in the to-do list.

- Fix the problem that the custom chart of project overview cannot be loaded.

## [1.2.0-alpha] - 2021-12-10

### Added

- Add a Gantt chart view to the workbench.
- Batch moving of work items to other projects is supported.
- Kanban configuration supports custom filter.
- Kanban supports subtasks that have been completed hidden in historical iterations.

### Changed

- Optimize the expected start, expected end, actual start and actual end time in the format of "year month day hour minute".
- Optimize the list of all work items, and drag and drop to customize the column width experience.


### Fixed

- Fix the problem that the quick filter operator did not allocate data correctly.


## [1.1.0] - 2021-11-05

### Added
- Add work calendar function.
- Gantt chart supports viewing by epic view.
- Gantt chart supports custom task order.
- Gantt chart supports custom column fields.
- Gantt chart supports rapid creation of work items and sub work items.
- Add system fields "actual start time" and "actual end time" for work item details.
- The bar chart on the right side of the Gantt chart shows that the delay can be rendered according to the actual start time and actual end time fields.
- Add Gantt chart at the organizational level.
- Add the system field "estimated time" for work item details.
- Add work calendar function.
- New work log function.

## [1.1.0-alpha] - 2021-09-18

### Added

- Support custom collaboration charts.
- Project reports support inserting custom charts.
- Project overview supports adding custom charts.
- Gantt chart supports viewing tasks with multiple iterations.
- Gantt chart supports viewing by sprint view.
- Gantt chart supports sorting by handler and estimated start time.
- Notification configuration supports custom personnel fields.
- Notify the configuration support to select the main person in charge.
- Issue notice to increase followers.
- Support sending daily work reminders.
- The page supports displaying by role configuration field.
- The page supports field cascade settings.
- The copy issue supports mandatory field verification.
- Turn on iteration support to check whether story points and work hours are estimated.
- All issue lists support field ascending and descending sorting.
- All issue lists support setting the problem field value blank.
- The state machine supports conditional verification of dragging parent tasks when child tasks have been completed.


### Changed

- Issue log details support the display of epic summary.
- Issue details support displaying the number of comments.
- Time spent optimizing the recording of issue log details.
- Issue Log add environment and main person in charge.
- Optimize the content display of issue comments and replies.
- Optimize the to-do list. By default, only the current sprint will be expanded.
- Export issue filter add filter by summary.
- Export issue style optimization.
- Workbench style optimization
- Optimize the workbench and click issue details to open a new tab.
- Add common filter "I handle" to all issues.
- The optimization to-do issue can be dragged into the unexpanded iteration.
- Optimize agile message notification content.
- Optimize interface loading dynamics.

### Fixed

- Fixed a problem where the profile default value was not displayed when creating an issue quickly.
- Fix some editing defects in rich text editor.
- Fixed the problem of scrolling white pages left and right on the Gantt chart.
- Fix the problem that the input and output directions of the issue chain are wrong when creating the issue.
- Fixed the problem of adding static list table field to project report.
- Fix the problem of duplicate name verification of personal filter.


## [1.0.0] - 2021-06-18

### Added

- Add input prompt.
- Subtasks support issue type conversion.
- Add daily workload per person chart.
- Add personal workload statistics.
- All issues support custom list field display, order, and column width.
- Optional replication of fields is supported when adding replication problems.
- The assignee can be selected when adding quick creation questions
- Add one click collapse to project report.
- Issue detail support rapid defect creation.
- New test execution status change triggers issue status change.
- New branch merging triggers associated issue status change.
- Adding order of module list.
- Compatible with Safari browser.

### Changed

- Support to switch Kanban when configuring Kanban.
- Optimization issue detail association issue.
- Optimize the display of comments in reverse order by default.
- Prompt optimization for creating required items quickly.
- Optimize quick creation of subtasks and automatically open details.
- Optimization issue details more button order.
- Optimization issue comment send message.
- Optimize state machine page.
- Optimize time display format.
- Load slowly when there are too many optimization custom issue type value sets.

### Fixed

- Fix the problem that UI / UX files cannot be previewed after uploading.
- Fix the problem of moving left and right time period side length of Gantt chart.
- Fix the problem of accidental disorder in state custom sorting.



## [0.25.0] - 2021-04-09

### Added

- The problem details support uploading UI&UX compressed files and support online viewing (support zip, rar4, tar, tar.gz).
- "Report to me" is added to the personal workbench.
- Add "My Handling" to the personal workbench.
- Moving issues support forwarding items.
- The problem list supports tiled list viewing.
- You can view the source branch of the problem related branch.
- The problem details development item supports viewing the associated branch and its commit.
- Add multi-select personnel controls for custom fields.
- Platform project types support association restrictions based on background services.
- Custom and pre-defined fields support the maintenance of default values, and support the synchronization of default values.
- Requests/issues/features support replies and send notifications to related parties.
- Import and export support to save frequently used templates.
- Add devops report for project report.
- Issue status supports sorting.
- Regularly notify relevant personnel of issues/sprint overdue information.
- Issue import supports importing custom fields.
- Added "operation dynamics" to the project overview.
- Menu access API supports menu dimension control.
- Increase the organization level Kanban template.
- Added organization layer state machine template.
- All personnel type fields support pinyin search matching.

### Changed

- Module setting page can adjust the order and load more.
- Single-selection and multiple-selection value lists for custom fields support searching.
- Attachment upload supports multi-part upload.
- New questions support setting status.
- Iterative planning kanban adds multiple screening fields.
- The notification object is not required when sending a webhook notification.
- The rich text editor adjusts and adds support for table operations.
- Some page style optimizations.
- Partial report optimization.

### Fixed

- Fix the inconsistency of issue data when dragging the to-do list quickly.
- Fix the problem that the custom flow of the initial state setting of the state machine does not take effect.
- Fixed the issue of notification and email being triggered when the dashboard only drags and does not change the status.
- Fixed the problem that multiple duplicate problem items were created by clicking quickly when creating a problem quickly.
- Fix the status change of the Gantt chart problem, and the postponement information is wrong.


## [0.24.0] - 2020-12-24

### Added

- Workbench - my issues increase my star.
- Support star issues.
- Add Gantt chart function for project manager to schedule tasks.
- Backlog issues support batch deletion.
- Issues support association or new test cases.
- Sprint Lane added to story map.
- Add performance features.
- Add project report.
- Add trigger, can set automatic change operator when automatic change, send message notification.
- Kanban support problem delay reminder.
  

### Changed

- Agile state machine supports user-defined state flow, state change notification, flow condition restriction, parent-child problem state linkage, and automatic attribute change after state flow.
- Optimize page field configuration.
- Optimize story map style.
- Partial views style optimization. 
- Partial report optimization.


### Fixed

- Fix branch management filtering error.
- The repair backlog is invalid according to the operator.
- Fix batch modification repair version error.
- FIX configuration Kanban mobile reporting error.
- Repair work list - all problems save filter error.  




## [0.23.0] - 2020-09-30

### Added

- Add Project Overview so that project members can quickly see the progress of the iteration.

### Changed

- Optimize one-click expansion of all problems.
- Optimize the filtering of the Work List.

### Fixed

- Fixed incorrect template of in station messages and mails sent when issue was created, assigned and completed.


## [0.22.0] - 2020-08-01

### Added

- The page of worklist - all issues are presented at issue level.
- Batch modification of issue is supported for the page of worklist - all issues.  
- The page of worklist - all issues support all fields filtering, including all predefined fields and custom fields.
- Agile Kanban supports full screen display.
- Agile Kanban supports viewing multiple iterations.
- The import issue can be imported according to the parent-child level of task - subtask and story - subtask.
- Agile Kanban supports custom state order.
- Add preset nails and enterprise wechat webhook templates for agile messages.  

### Changed

- The to-do epic sidebar is optimized to not display completed epics.
- Optimize subtask details page: you can directly see the summary of the parent task.
- Optimize agile service permissions.
- Optimize the export problem performance.
- Optimize team member workload display in the page of to-do.
- Partial views style optimization. 
- Partial report optimization.

### Fixed

- Fix the duplicate display of the sub text box picture paste of issue details.
- Fix create state interface permission check error.


## [0.21.0] - 2020-03-06

### Added

- Configure Kanban to support deletion status.
- Import problem supports importing parent-child relationship. Users can import sub tasks at the same time when importing stories or tasks.
- Agile message notification supports email mode.  

### Changed

- Optimize agile Kanban performance.
- Optimize work list performance.
- Optimization configuration Kanban status set to finished saving does not take effect.
- Optimize the configuration of Kanban custom state order.
- Optimize to-do mass drag problem quantity display.
- Optimize the issue link page style.
- Optimize custom field page styles.  

### Fixed

- Fix problem with story map full screen menu bar.
- Fix the problem of no data after the iteration planning workbench is refreshed.
- Fix issue details remaining estimated time names showing incorrect problems.
- Fixed a problem where the story map epic could not view comments in certain situations.
- Fix the problem of setting up the agile module owner to display undefined.
  
  
## [0.20.0] - 2019-12-20

### Added

- Support filtering by clicking the assigneer on the backlog.
- Story map supports filtering by no version in demand pool.
- Filtering questions supports selection of historical version and historical sprint.

### Changed

- Subtask support association knowledge.
- Kanban default swimlane configuration adjusted to assigneer.
- Optimized configuration Kanban can also be configured when there is no sprint.
- Remove text at top of sidebar.
- Unified field explanation icon and description style.
- Uniform font color and size.

### Fixed

- Fix knowledge link jump problem in issue detail.
- Fix missing priority field for issue list.
- Fix the bug that issue details quickly create subtask will create repeatedly when multiple clicks. 


## [0.19.0] - 2019-10-18

### Added

- Added in the user story swimming lane click on the name to see the details.
- merge issue services, state machine services, basic services to agile services.
- New restrictions on story type conversion, if there are subtasks, can only be converted to tasks.
- Add a preview of attached pictures, pdf, word, excel and other files.
- Create a template of bug type and give the initial template.

### Changed

- Global style adjustment.
- Cancel the last level of breadcrumbs and leave it at the menu level only.
- Optimize data loading for drop-down selection.
- Operation optimization after story map reconstruction.
- Create problem custom fields for load optimization.
- Worklist to-do UI optimization.
- Question management list search refactoring.

### Fixed

- Fix the modification parent task while modifying the sprint.
- Fixed the problem of incorrect return of personnel information when all member tasks are subtasks.
- In fixing the problem, modify the problem module. The module check box checks whether the status is the last one.
- Fixed module deletion in module management when mobile problems could not access other modules.
- Fix the problem by setting the status page to filter the status list to be empty and unable to create the status.
- The repair details page has changed the priority, and the list page has not been updated.

## [0.18.0] - 2019-06-21

### Added

- Added a new issue type feature.
- Story map reconstruction mainly includes: supporting the planning of new issue type feature; supporting the planning of all stories; showing only no lanes and versions of lanes.
- You can quickly create subtasks in stories.
- Added custom fields for new staff types.
- Added activity log of custom field modification.

### Changed

- Optimizing the performance of Partial Interfaces.
- For users who have been disabled on the platform, the personnel list is no longer displayed.
- The parent task can see progress bars for all subtasks.
- Partial views style optimization. 
- Partial report optimization. 

### Fixed

- Fixed special character error of description field in problem import.
- Fixed the Bug that a version can be created when it has an empty name.  

## [0.17.0] - 2019-05-24

### Added

- When the program completes PI, sprint under PI will be completed automatically, and the front end of team receives prompt.
- Program - Project settings can view project information.
- Feature management list of program function upgrade, advanced search.
- PM can view feature backlog of 3 PIs in the program Roadmap.
- Program can view working hours based on their work calendar.
- Users can create bugs directly in stories.
- Program members can view dependencies between teams and sprints through bulletin boards.
- Support sorting in feature query mode.
- Program kanban add fast search.

### Changed

- Issue association relationships show associated test cases.
- Issue import template add component, sprint fields.
- Optimizing the width and narrow styles of issue details pages.
- Custom field optimization related optimization.
- Partial views style optimization. 
- Partial report optimization.

### Fixed

- Repairing the PI display bug of epic screening.
- Repair display bug of ART list time.


## [0.16.0] - 2019-04-19

### Added

- Add ART setting of program, which supports the creation, modification, opening, deactivation of ART, and display of PI lists under ART.
- Add feature list of program, which includes two modes: plan mode and query mode, and supports feature creation.
- Add program board, which supporting feature movement, display, etc.
- Add setting of program board, including column and status configuration.
- Add project setting of program, which supports the modification of project code.
- Add ART calendar of program, which supports viewing in progress ART PI planning and sprint planning under PI.
- Add PI objective of program, including list and card, supports creating, modifying, deleting and querying PI objective.
- Stories in team of program can associate features of States to be processed or processed.
- After PI is turned on in the program, sprint is generated synchronously for each team in the program, and no new sprint is allowed to be deleted or created.
- Create Issue/Edit Issue Page，loading custom field list，to adapt to the style of width，save and update field value.

### Changed

- Project members can view the burnout chart on the project home page.
- Project members can view unassigned tasks on the project home page and it supports paging.
- When a subtask in a story is moved to the next sprint, remember the previous state.
- Partial views style optimization. 
- Partial report optimization.

### Fixed

- Data display in different dimensions of epic report.
- Incorrect search by name in problem management.
- Repair the 5.1 holiday adjustment
  

## [0.15.0] - 2019-03-22

### Added

- Priorities are customized and arranged in actual order at the organizational level.
- Advanced search function of issue Management.
- Import and export issues.
- Limitations in Work-in-Process in active sprint.
- DEMO function addition.  

### Changed

- When searching for queries, display fields for fields in a list of values.
- Issue management supports custom filter display fields.
- Backlog scheduling and display performance optimization.
- When creating subtasks in stories, story information is displayed on the page.
- Issue details page, required tips for registering work log.
- Only allow yourself and the project owner to modify the reporter.
- Partial views style optimization. 
- Partial report optimization. 

### Fixed

- Cancel collection function in no swimming lane board.
- Backend SQL error reporting in search of quick search pages.
- The same relationship of issue link can be associated with the same issue many times in issue details page.
- Burn-out diagram does not correspond to the number of remaining issues/remaining time in backlog.
- The issue counting bug of version in backlog.  


## [0.14.0] - 2019-02-22

### Added

- Warning hints for residence time of cards in board.
- Repeated prompt for sprint name.
- Board permission restriction.
- Support association when creating issues.
- Story point and remain time support 0.5 decimal points.

### Changed

- Show the completed issue and cross out the number.
- The stories which all the subtasks completed are sorted under the story swimming lane.
- Description optimization of active logs.
- Add or modify filter for issue links, issue management, version details, release versions, modules and quick filter.
- Under the epic swimming lane, the board supports viewing stories and reflects the relationship between stories and subtasks.
- Display optimization of task cards after filtering in different swimming lanes.     
- Optimized modification of logic problems related to creating modules.
- The affected version can be selected to the full version.    
- Partial views style optimization. 
- Partial report optimization.

### Fixed

- The problem of incorrect number of sub-texts when pasting network pictures.
- Repair the problem of double counting the number of burnout issues when closing the sprint.
- Issue is not refreshed synchronously in the backlog when modified the name of the epic.
- There is error when version or sprint is null in statistic chart.
- Work Calendar Non-Holiday Computing bug.
- Status color errors in story point statistics.


## [0.13.0] - 2019-01-11

### Added

- Version details filtering functionality.Version details support advanced filter.
- The statistical graph adds label dimensions, and adds sprint, version, and time filter conditions.
- The board name modifying functionality in board setting, and add the verification of duplicate name.
- Issue details narrow styles add log information.
- The issue is supported by filling in remaining time and story points when creating.
- Increase epic name, component name, and version name verification.

### Changed

- Select issues user action monitoring optimization in backlog view.
- Partial view style optimization.
- Optimize burndown chart calculation logic.

### Fixed

- The statistics of the sprinters in the planned backlog view are repeated.
- Cumulative flow graph dirty data repair, need to be manually called for repair.
- The creation state and the deletion state cause the state machine draft configuration table to generate dirty data and the publication is unavailable.
- The date selector's holiday display error.

## [0.12.0] - 2018-12-14

### Added

- Version details filtering functionality.Version details support advanced filter.
- Active sprint issues drag sort functionality.Issues support drag sort in active sprint's board view.
- Active sprint support filtering of team members.
- Work calendar added 2019 statutory holiday data.
- New issue link for version details.
- Version add the field of expected release date.

### Changed

- Issue creators can delete issues which they created.
- The state is automatically changed to the default state after the task is converted to a subtask.
- When the sprint is complete, the unfinished subtask moves with the parent task to the next sprint.
- Create a quick search of the relationship field to display the name changed to Chinese.
- The workload of the sprints in the backlog list is changed to the total number of issues, the remaining number of issues, the total task hours, and the remaining task hours.
- The field of end date changes to the field of expected release date when you create new version.
- User need to enter the actual release time when user release the version.
- The board sets the column constraint to be modified only by the project owner.
- User can only select the planning version to merger other version.
- Partial views style optimization.

### Fixed

- Statistics incomplete issues count error in version details view.
- The issue details priority drop-down list is incomplete.
- Failed to create version name in Chinese.

## [0.11.0] - 2018-11-16

### Added

- Issue type customization functionality.new issue-service, issue-service support custom issue type, support issue type icon customization, custom issue type will be applied in agile-service.
- Issue priority Customization functionality.The issue-service supports custom problem prioritization, and the custom issue priority will be applied in the agile service.
- Issue state machine functionality.new state-machine-service, issue status update, creation, and deletion are controlled by state-machine-service.

### Changed

- Issue management adds new field display, field search, field sorting.
- Issue management supports custom filtering.
- The Issue management view subtask is also displayed in the list.
- When the Issue in the active sprint is dragged to another location then the problem and its subtasks are all restored to the state machine initial state.
- Issue details form page optimization.
- Product global illustration optimization.
- Active sprint view display optimization.
- Backlog epic count details optimization.
- Calendar style and operation optimization.
- Calendar workdays and holidays return the current year and next year data by year.
- The issue link list shows the assigner message.
- The iteration speed graph is not counted the not start sprint.

### Fixed

- The data after the work log time registration in the issue management is not updated.
- The version, epic sorting error in backlog view.
- Active sprint and iteration work bench remaining time calculation error.
- The version report cache was not updated in a timely manner.
- Drag the issue in multiple states will show white screen in the active sprint.

## [0.10.5] - 2018-10-22

### Added

- In-site notification functionality.Users can assign corresponding notification objects to the issue creation,issue assignment and issue resolution at the organizational level.
- Time zone calendar functionality.Users can set time zones,holidays and workdays at the organizational level.It will be apply in agile-service.
- Version management search functionality.The version management list adds the field search functionality.
- Component management search functionality.The component management list adds the field search functionality.
- Sprint Workday functionality.Users can set the working day and non-working day of the current sprint when user start sprint and selecting date.
- Burndown chart expectation value workday screening functionality.Users can display the expected value on weekdays and non-working days when viewing the burndown chart.

### Changed

- Users can save the input data by pressing Enter or clicking on the blank space when modifying the issue information.
- Add a guided prompt page after the sprint create.
- Changes the icon of stories,tasks,epics,subtasks and bugs.
- Use the calendar setting for the remaining time of the active sprint.
- Active sprint switch table button style modification.
- Add a verification to the issue in the story map.
- Optimize creation issue request in the backlog list view.

### Fixed

- Users can create a issue link without entering a value in issue's detail.
- Users converted a issue to a subtask that the issue's status color is incorrect.
- The issue's details page component is not aligned.
- The release version page link to uncompleted issue list filter error.
- The link address Chinese has not been encoding to cause the request to be repeated.
- The subtask icon in the list of issues in the release version is incorrect.
- The no epic's issue card's style error in active sprint view.
- In burndown chart report view,user click on the subtask link is the parent task details.
- Link to issues management in the epic and version burndown chart, return to page 404.
- The table of iterative workbench sprint details loads all the data at one time, and repeatedly loads the data when the page is clicked.
- The number of uncompleted issues's counted errors when the version was released.

## [0.10.0] - 2018-09-24

### Added

- Epic burndown charts functionality.Users can select epic burndown charts in the report view. Charts and reports show the progress made by teams in different epics and predict the trend of future sprint completion.
- Version burndown charts functionality.Users can select version burndown charts in the report view. Charts and reports show the progress made by teams in different epics and predict the trend of future sprint completion.
- Sprint workbench functionality.Users can check the status, priority, assignee, and type distribution of the issues in the sprint workbench.
- Report workbench functionality.Users can view real-time data of the cumulative flow chart, assignee distribution chart and other charts in the report workbench.
- Story map export functionality.
- Full screen operation of story map functionality.

### Changed

- The sliding functionality of the story map can be more fluid.
- The story map can record its location when moving the issue.
- The story map can record its location when the requirements pool is dragging a issue.
- Partial view memory optimization.
- The burndown chart and sprint report in the report can build a cache to retain the last selected sprint and its units.
- Modify the style of adding state in the board configuration.
- Cumulative flow graph to obtain time function optimization.
- The version progress in the dashboard filters out the archived version.
- Creating issue links in the Settings adds duplicate validation.
- Story maps support drag up and down sorting in different swimlane.

### Fixed

- The page stack phenomenon occurs when user drag an issue to a column with multiple states in active sprint view.
- When user drag issue to sorting in active sprint view, data of this page will occurs delay problem.
- Creation issues take too long to execute in backlog view.
- Issue's descriptions is a string with formatting in Excel when user export Excel in issues manager view.
- The sprint burndown chart failed under some conditions which according to the number of issues.

### Removed

- The paging functionality does not show the paging toolbar if it is less than 10.


## [0.9.5] - 2018-08-31

### Added

- Dashboard functionality.Users can customize the dashboard in the home page.The dashboard contains: burndown chart, version progress, epic progress, my unfinished issues and more.
- User story map functionality.User story map is based on epic, planning and managing issues according to version and sprint dimensions.
- User story map swim lane functionality.Users can select none, version, sprint to divide the swim lane and record the user's choice of swimlane.
- User story map demand pool functionality.Users can filter all unallocated epic's issues in the demand pool of the user story map.
- User story map issue drag functionality. Users can drag the issue between different epics, versions, sprints, or drag to the map board in the demand pool.
- Report chart caching functionality: Cache the charts in the report via Redis.
- The issues in the chart can transfer to issue management view (without subtasks).
- Unit tests for backlog, active sprints, release version and component management.
- Record the swim lane settings of the user's active sprint corresponding to the board.
- Issue management view export Excel contains subtasks.

### Changed

- Some view styles are modified.
- Partial view memory optimization.
- Cumulative flow chart query optimization.

### Fixed

- Drag issue repeatedly generates a data log in the backlog view.
- The backlog view has no issue cause sprint data does not receive.
- Dragging issues to an unsupported version failed in the backlog view.
- Dragging issues to an unsupported version failed in the backlog view.
- When there are column constraints of the board configuration in the active sprint view, the constraints can be skipped directly by modifying the state of the backlog view.
- Failed to delete the version in the version management view.

## [0.9.0] - 2018-08-17

### Added

- Version drag sort functionality.Users can drag the version to sort versions in the version management view and the backlog view.
- Epic drag sort functionality.Users can drag the epic to sort epics in the backlog view.
- Quick Search sort functionality.Users can drag the quick search and sort quick searchs in the set-up quick search view.
- Sprint speed chart functionality.Users can select story points, issues counts,remaining time to see issues's resolve and unresolved proportional histogram of different sprints.
- Epic report functionality.Users can select different epics through story points, issues counts, remaining time to see the current sprint resolve, unresolved, unresolved and unpredictable issues.And users also can see the corresponding summary data.
- Issue statistics chart functionality.Users can view the issue statistics chart in the project according to the assignee, component, issue type, fix version, priority, status, sprint, epic and resolve results.
- Issue's details back to history view functionality.Users can back to the original page after clicking on the issue's details in any views.
- Added agile-service unit testing based on Spock.
- Added create branch functionality in the issue's details operation.
- When users modify the state to completed and the issue resolve log will be generated.
- Modify sprint name have length limit.

### Changed

- Agile-service message propagation is modified from Kafka to Saga.
- Optimize the request time of the version report.
- Optimize the equest time of the burndown report.
- View style adjustment for backlog view.
- Optimize the problem of slow loading of epics and versions in the backlog view.
- Data logs processing logic refactoring.
- Modify version status style.

### Fixed
- Memory overflow for backlog view.
- Burndown chart data are inconsistent.
- Cumulative flow chart data are inconsistent.
- Data display is inconsistent after component management creation component.

### Removed
- Details of the issue are displayed on the left side of the narrow work log and activity log.
- Cumulative flow graph statistics for epics and their sub tasks.

## [0.8.0] - 2018-07-20

### Added

- Branch management functionality.Users can creating,merging and viewing branch information from Gitlab remote repository branches associated with issues in issue detail view.
- Version report functionality.The version report shows your team's progress in completing the version. The version report can be filtered based on the remaining estimated time, story points, and issue counts. The version report will also be based on your team's since the release. The average progress (speed) and the estimated remaining workload show you the predicted release date.
- Cumulative flow diagram functionality.The cumulative flow diagram is an area graph showing the status of various work items for the application, version and sprint. The horizontal x-axis represents time, the vertical y-axis represents the problem count. Each colored area of the chart is equivalent to the issue change listed on the panel, and the cumulative flow diagram can be used to identify bottlenecks.If your chart contains areas that are vertically widened over time.A column equal to the widened area usually becomes a bottleneck.
- Test type issue functionality.Test type issue are used in the "Test Manager" module, from which users can create test type issue for managing test cases.
- Project default setting functionality.The project owner can set the default assignee and the default priority of the project's issues. If user create issue havn't assignee or priority.System will create the default assignee and priority according to the project settings. The default priority of the project is lower than the default assignee of the component.
- User default board functionality.If user selects the board,system will recordeing what your select.When the user enter  active sprint view,the board selected by the user will be displayed.
- Issues export Excel functionality.Users can filter out issues according to the selected conditions then export to the table.
- Issue converted to subtask functionality.Users can convert other types of issues into subtasks.In particular, the story issue is converted to a subtask.And the story point is modified to 0.
- Issue copy functionality.Users can copy the issue by selecting parameters:question link and subtask.Copy the issue will generate a copy type link with the original issue.
- The release log can be viewed in the version view.
- Version log export Markdown document functionality.Users can export issues informations to Markdown document in the release version of the version log.

### Changed

- The epic type issues defaults to the initial color modification.
- During update the version association of the issue, you cannot delete the version association that has been archived.
- Optimize the search interface and modify the trigger logic.
- Optimize the burndown chart data query interface.
- The release time display field is modified from the start time to the release time.
- Hidden no lanes are assigned when no issues on the board.
- The issue card on the board can view epic information.
- Modify the active sprint first in the menu order.
- Modify the location of the epic name in the issue details.
- Issue details style optimization.
- Backlog list view style optimization.
- Remove the project code duplicate name check in the project settings.
- Issues is arranged to refresh each time in a certain order on the board.
- When you select issues in the backlog list, if you click on the details of one of the issue, you can select multiple issues based on the issue.
- The project creates an initial test type issue.
- Add the action of converted to subtask, copy operation in the issue details.
- Release issues can be linked to issues management by clicking on the link.
- The report view can be associated with a list of issues and details of each issue.
- The report view adds a version report, cumulative flow diagram entry.
- Report selects switches add the version report and the cumulative flowchart.
- The issues in the sprint report can be viewed by grouping in the report to the issues management view.
- The swim lane type in the board settings is added to display issue.

### Fixed
- Issue details Anchor location is not accurate.
- The epic color in the issue details does not match the epic color.
- When the issue is based on the story display and selecting only my issues.The parent task does not belong to the same assignee's swim lane display defect.
- Select the board style question.
- Active sprint story point statistics color error.
- A Easy way to create issues with Caton.
- The filter user limits 20 people when the filter is created.
- The issue details select the manager and reporter component issue.
- You can only select up to 400 issues when selecting a link issue.
- Creationing issue link by search result failed.
- The backlog list issues is dragged into the version but the list of issues in the corresponding version is not refreshed in real time.
- The list shows the data operation load delay.
- The backlog list issues is dragged to the sprint but the sprint's users information is not updated.
- When the issue summary is switched during editing, the contents of the edit box will be cleared.
- The problem is dragged into the version but not create logs.
- New project creation issue but issue number starting from 2.
- Issue Management quickly creates epics but epic havn't epic name.
- The rich text editor failed to break words in the case of multiple English.
- The story issue changed to other types but the issue's story points not set to 0,also when  the epic type issue changed to other types then the issues that belongs to the epic was not updated.
- The board in Firefox browser shows a style error.
- The board's swim lane error  when the epic issues does not exist.