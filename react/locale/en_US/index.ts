/*eslint-disable*/
export * from './common';
export * from './scrumBoard';
export * from './backlog';
export * from './issue';
export * from './storyMap';
export * from './gantt';
export * from './version';
export * from './projectReport';
export * from './issueType';
export * from './page';
export * from './setting';
export * from './stateMachine';
export * from './performance';
export * from './chart';
export * from './projectOverview';
export * from './search';
export * from './components';

const en_US = {
  refresh: 'Refresh',
  filter: 'Filter',
  create: 'Create',
  save: 'Save',
  confirm: 'Confirm',
  cancel: 'Cancel',
  delete: 'Delete',
  edit: 'Edit',
  relation: 'Ralation',
  copy: 'Copy',
  add: 'Add',
  createSuccess: 'Create Success',
  editSuccess: 'Edit Success',
  deleteSuccess: 'Delete Success',
  createFailed: 'Create Failed',
  editFailed: 'Edit Failed',
  deleteFailed: 'Delete Failed',
  required: 'This field is required',
  disable: 'Disable',
  enable: 'Enable',
  name: 'Name',
  code: 'Code',
  'network.error': 'Network Error',

  "issue.detail": 'Detail',
  "issue.des": 'Description',
  "issue.attachment": 'Attachment',
  "issue.doc": 'Docs',
  "issue.commit": 'Commit',
  "issue.log": 'Work Log',
  "issue.data_log": 'Log',
  "issue.sub_task": 'SubTask',
  "issue.bug": 'Bug',
  "issue.link_task": 'Issue Link',
  "issue.branch": 'Branch',
  "issue.branch.create": 'Create Branch',
  "issue.commit.create": 'Create Commit',
  "issue.log.create": 'Create Log',

  "learnmore": "learn more",

  'issueType.title': 'Issue Type',
  'issueType.create': 'Create Type',
  'issueType.tip': '工作项类型是将敏捷服务和测试服务中的工作项类型进行统一管理。',
  'issueType.name': 'Name',
  'issueType.res': 'Description',
  'issueType.scheme': 'Related Schemes',
  'issueType.delete.tooltip': 'Confirm delete this issue type?',
  'issueType.action.delete': 'Delete Issue Type',
  'issueType.createDes':
    'Create a issue type by entering the name and description of the issue type and selecting or uploading an icon for the issue type.',
  'issueType.label.name': 'Name',
  'issueType.label.des': 'Description',
  'issueType.label.icon': 'Icon',
  'issueType.name.check.exist': 'Issue type name already exist.',
  'issueType.delete.forbidden':
    'The current issue type is being used by the issue and cannot be deleted.',
  'error.issueType.update': 'The update failed. Please refresh and try again.',

  // stasteMachineScheme
  'stateMachineScheme.title': 'State Machine Scheme',
  'stateMachineScheme.create': 'Add State machine scheme',
  'stateMachineScheme.name': 'Name',
  'stateMachineScheme.des': 'Description',
  'stateMachineScheme.project': 'Project',
  'stateMachineScheme.issueType': 'Type',
  'stateMachineScheme.stateMachine': 'State Machine',
  'stateMachineScheme.operation': 'Operation',
  'stateMachineScheme.createName': 'Please enter the state machine sheme name',
  'stateMachineScheme.createDes':
    'Please enter a detailed description of this state machine sheme',
  'stateMachineScheme.edit': 'Edit state machine scheme',
  'stateMachineScheme.manage': 'Project management process plan',
  'stateMachineScheme.manageDes':
    'This Cloopm service desk IT support status process plan was generated for the project ITSM.',
  'stateMachineScheme.add': 'Add State machine',
  'stateMachineScheme.next': 'Next step',
  'stateMachineScheme.connect': 'Connect problem type to state machine',
  'stateMachineScheme.connectIssueType': 'Issue type',
  'stateMachineScheme.connectedStateMachine':
    'Currently connected state machine',
  'stateMachineScheme.pre': 'Previous step',
  'stateMachineScheme.finish': 'Finish',
  'stateMachineScheme.cancel': 'Cancel',
  'stateMachineScheme.delete': 'Delete state machine scheme',
  'stateMachineScheme.deleteDesBefore': 'Are you sure you want to delete ',
  'stateMachineScheme.deleteDesAfter': 'Note: This state machine scenario will be removed from all projects.',
  'stateMachineScheme.conflictInfo':
    'This problem type is already associated with another state machine. If you need to update this association, the previous results will be overwritten.',
  'stateMachineScheme.tips': 'Note: This state machine scheme is being used.You are editing #draft state machine# , if the revised draft needs to take effect, click #publish# .',
  'stateMachineScheme.publish': 'Publish',
  'stateMachineScheme.announcing': 'Announcing',
  'stateMachineScheme.deleteDraft': 'Delete Draft',
  'stateMachineScheme.original': 'Show Original',
  'stateMachineScheme.draft': 'Edit Draft',
  'stateMachineScheme.delete.draft': 'Delete Draft State Machine',
  'stateMachineScheme.delete.des': 'Confirm delete the currently edited state machine draft?',
  'stateMachineScheme.title.matching': 'Match issue type and status',
  'stateMachineScheme.title.publish': 'Publish the state machine',
  'stateMachineScheme.publish.des': 'In order to make some issue compatible with the new state machine, the current state of these issue requires a new match.',
  'stateMachineScheme.publish.noMatch': 'In this modification, all issue types and states are automatically migrated.',
  'stateMachineScheme.targetStatus': 'New State',
  'stateMachineScheme.sourceStatus': 'Current State',
  'stateMachineScheme.des.none': 'No description',

  // 优先级
  'priority.title': 'Priority',
  'priority.create': 'Add Priority',
  'priority.edit': 'Edit Priority',
  'priority.name': 'Name',
  'priority.des': 'Description',
  'priority.color': 'Color',
  'priority.list.tip': 'The following list shows the priority you are currently using, in order of highest to lowest, you can also change the display order by dragging up and down.',
  'priority.create.name.placeholder': 'Please Input Name',
  'priority.create.des.placeholder': 'Please Input Description',
  'priority.create.color.error': 'color exist',
  'priority.create.name.error': 'name exist',
  'priority.delete.title': 'Delete Priority',
  'priority.delete.unused.notice': 'Note：This priority will be removed from all used tickets.。',
  'priority.delete.used,notice': 'Note: This priority will be removed from all used event tickets.Please select a new priority for the affected event list.',
  'priority.delete.chooseNewPriority.placeholder': 'Please choose a new priority',
  'priority.default': '(Default)',
  'priority.delete.notice': 'Note: This priority will be removed from all used issues.',
  'priority.delete.used.notice': 'Please choose a new priority for the affected issue.',
  'priority.delete.used.tip.prefix': 'There are currently ',
  'priority.delete.used.tip.suffix': ' issues that are using this priority.',
  'priority.disable.title': 'Disable Priority',
  'priority.disable.notice': 'Note: Your issue will not be able to choose this priority after disable.',

  'state.name': 'Name',
  'state.des': 'Description',
  'state.stage': 'Stage',
  'state.stateMachine': 'State Machine',
  'state.title': 'State',
  'state.create': 'Create State',
  'state.edit': 'Edit State',
  'state.delete': 'Delete State',
  'state.delete.tip':
    'Warning: Cannot be restored after deletion. Please perform data backup first.',
  'state.name.required': 'Name is required',
  'state.tips': 'Help identify a stage of the life cycle of the problem',
  'state.tips2': 'When you start processing the problem, from #Pending# to #Processing#, then, when you have completed all the work, you are in the #Completion# phase.',

  'stateMachine.name': 'Name',
  'stateMachine.related': 'Related Scheme',
  'stateMachine.title': 'State Machine',
  'stateMachine.create': 'Create State Machine',
  'stateMachine.edit': 'Edit State Machine',
  'stateMachine.tab.graph': 'Graph',
  'stateMachine.tab.text': 'Text',
  'stateMachine.des': 'Description',
  'stateMachine.state.delete': 'Delete State',
  'stateMachine.transfer.delete': 'Delete Transfer',
  'stateMachine.state': 'State',
  'stateMachine.state.add': 'Add State',
  'stateMachine.state.edit': 'Change State',
  'stateMachine.transfer': 'Transfer',
  'stateMachine.transfer.add': 'Add Transfer',
  'stateMachine.transfer.display': 'Display Transfer',
  'stateMachine.transfer.name': 'Name',
  'stateMachine.transfer.des': 'Description',
  'stateMachine.transfer.source': 'Start State',
  'stateMachine.transfer.target': 'Target State',
  'stateMachine.transfer.page': 'Page',
  'stateMachine.list.tip':
    '只有未关联状态机方案的状态机才能删除。',
  'stateMachine.transfer.edit': 'Edit Transfer',
  'stateMachine.transfer.delete.tip': '请选择你需要删除的转换。',
  'stateMachine.delete': 'Delete State Machine',
  'stateMachine.condition': 'Vondition',
  'stateMachine.Verification': 'Verification',
  'stateMachine.processor': 'After Processor',
  'stateMachine.config': 'Config',
  'stateMachine.condition.des': 'Conditions can control which users can perform a conversion under what circumstances.If the condition is not met, the user will not see the transition button on the interface where the problem is viewed.',
  'stateMachine.condition.guide': 'Don\'t know where to start?',
  'stateMachine.condition.link': 'Starting from here',
  'stateMachine.config_condition.add': 'Add Condition',
  'stateMachine.verification.des': 'The state machine verifier will check if the user\'s input is valid before the conversion is performed.For example: Verification ensures that the values ​​entered by the user on the converted page meet certain criteria.If the check is not satisfied, the post-processing function of the conversion will not be executed, and the problem will not proceed to the target state of the conversion.',
  'stateMachine.config_validator.add': 'Add Verification',
  'stateMachine.processor.des': 'After a conversion is performed, the system performs some operations immediately (hence the post-processing function), for example: updating a problem\'s fields and generating a modified record of the problem.',
  'stateMachine.config_postposition.add': 'Add Processor',
  'stateMachine.config.name': 'Name',
  'stateMachine.config.des': 'Description',

  'stateMachine.edit.draft.tip': 'Note: This state machine is being used.You are editing a draft state machine. If the revised draft needs to take effect, click Publish.After deleting the draft, the draft is backed up to the state machine that is currently in use.',
  'stateMachine.edit.deploy.tip': 'Note: This state machine is being used.If you need to make changes, click Edit.',
  'stateMachine.edit.avtive': 'View State Machine',
  'stateMachine.draft.delete': 'Delete Draft',
  'stateMachine.node.all': 'Let all the states change here',
  'stateMachine.node.name.start': 'Start',
  'stateMachine.node.name.all': 'All',
  'stateMachine.delete.confirm': 'Confirm delete the status {des} and its related conversions?',
  'stateMachine.node.deleteTip': 'Confirm delete？',
  'stateMachine.node.deleteInfo': 'Unable to delete status',
  'stateMachine.node.deleteDes': 'There are {count} issues using this state, and to delete the state, you need to convert the issues to a different state.',
  'stateMachine.transfer.deleteTip': 'Please select the transform you want to delete.',
  'stateMachine.transfer.deleteConfirm': 'Confirm delete transfer？',
  'stateMachine.draft': 'Draft',
  'stateMachine.publish': 'Publish',
  'stateMachine.publish.success': 'Publish Success!',
  'stateMachine.publish.info': 'Publish draft stateMachine fail',
  'stateMachineScheme.publish.warn': 'The current scheme has been modified, please refresh and try again.',
  'stateMachineScheme.tip': '状态机方案指的是，将状态机和工作项类型组合形成的方案，并关联项目。',
  'stateMachine.publish.des': 'The current state machine draft cannot be deleted because the state of the associated issue has been removed from your edits and the issue needs to be transitioned to another state in order to continue publishing.',

  'field.create': 'add field',
  'field.import': 'import field',
  'field.edit': 'edit field',
  'field.context': 'Context',
  'field.code.rule': 'code only allows numbers, letters and underscores',
  'field.code.exist': 'code already exists',
  'field.name.exist': 'name already exists',
  'field.type': 'filed type',
  'field.radio': 'radio',
  'field.checkbox': 'checkbox',
  'field.time': 'time',
  'field.datetime': 'datetime',
  'field.date': 'date',
  'field.number': 'number',
  'field.input': 'input',
  'field.text': 'text',
  'field.single': 'select',
  'field.multiple': 'multiple',
  'field.cascade': 'cascade',
  'field.url': 'URL',
  'field.label': 'label',
  'field.member': 'member',
  'field.default': 'defaultValue',
  'field.default.sync': 'sync defaultValue to',
  'defaultValue.sync': 'sync defaultValue',
  'field.decimal': 'decimal',
  'field.urlError': 'URL error',
  'field.useCurrentDate': 'Use Current Date',
  'field.useCurrentTime': 'Use Current Time',
  'field.dragList.tips': 'Please add a value to this field, you can change the display order by dragging up and down.',
  'field.value.null': 'Please set the field value list first.',

  'dragList.invalid': 'invalid',
  'dragList.active': 'active',
  'dragList.placeholder': 'Please enter an option value',
  'dragList.placeholder.code': 'Please enter an option code',
};


export { en_US as AGILE_EN_US };
