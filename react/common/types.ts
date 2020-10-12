import { ModalProps } from 'choerodon-ui/lib/modal';

export interface User {
  email: string
  enabled?: boolean
  id: string
  imageUrl: string | null
  ldap: boolean
  loginName: string
  realName: string
  name?: string
}

export interface ISprint {
  sprintId: string
  sprintName: string
  startDate: string
  endDate: string
  actualEndDate: string
  statusCode: string
}

export interface IPriority {
  colour: string
  default: boolean
  description: string
  enable: boolean
  id: string
  name: string
  objectVersionNumber: number
  organizationId: string
  sequence: number
}
export interface IVersion {
  versionId: string,
  name: string,
}

export interface IComponent {
  name: string,
  componentId: string
}

export interface ILabel {
  labelId: string
  labelName: string
}

export interface PI {
  id: string
  code: string
  name: string
  startDate: string
  endDate: string
  actualStartDate: string | null
  actualEndDate: string | null
  statusCode: 'todo' | 'doing' | 'done' | string,
}

export interface IStatus {
  id: string
  valueCode: 'todo' | 'doing' | 'done' | 'prepare'
  type: 'todo' | 'doing' | 'done' | 'prepare'
  name: string
}
export interface Priority {
  colour: string,
  default: boolean
  description: string,
  enable: boolean,
  id: string,
  name: string,
}

export interface IModalProps extends ModalProps {
  handleOk: (promise: () => Promise<boolean>) => Promise<void>,
  handleCancel: (promise: () => Promise<boolean>) => Promise<void>,
  close: (destroy?: boolean) => void,
  update: (modalProps: ModalProps) => void
}
interface CurrentProject {
  agileProjectCode: string,
  agileProjectId: number,
  agileProjectObjectVersionNumber: number,
  categories: Array<any>,
  category: string
  categoryIds: any,
  code: string
  createUserImageUrl: string,
  createUserName: string,
  createdBy: number,
  creationDate: string,
  editFlag: false,
  enabled: boolean,
  id: number,
  imageUrl: string,
  into: boolean,
  lastUpdateDate: string,
  lastUpdatedBy: number,
  name: string,
  objectVersionNumber: number,
  organizationCode: null | string,
  organizationId: number,
  organizationName: null | string,
  programName: null | string,
  projects: any,
  roles: any
  starFlag: boolean,
  type: any,
  typeName: any,
}
interface MenuType {
  category: string,
  id: string,
  name: string,
  orgId: string,
  organizationId: string,
  projectId: string,
  type: string,
}
export interface AppStateProps {
  currentProject: CurrentProject,
  debugger: boolean,
  expanded: boolean,
  guideExpanded: boolean,
  isUser: boolean,
  loadOrgDate: Function,
  loadSiteInfo: Function,
  loadUserInfo: Function,
  menuType: MenuType,
  siteInfo: any,
  userInfo: any,
  currentLanguage: string,
  currentMenuType: MenuType,
  currentOrginazationOrProjectId: string,
  getCurrentProject: Object
  getDebugger: boolean,
  getGuideExpanded: boolean,
  getMenuExpanded: boolean,
  getSiteInfo: Object,
  getType: string,
  getUserId: string,
  getUserInfo: Object,
  isAuth: boolean,
  isTypeUser: boolean,
}
export interface IIssueType {
  colour: string,
  description: string,
  icon: string,
  id: string,
  name: string,
  stateMachineId: string,
  typeCode: string,
}
export type IFeatureType = 'business' | 'enabler'

interface FieldOption {
  id: string,
  fieldId: string,
  code: string,
  value: string,
}
export interface IField {
  code: string,
  fieldOptions?: FieldOption[],
  fieldType: string,
  fieldTypeName?: string,
  id: string,
  name: string,
  system: boolean,
  extraConfig?: boolean,
}

export interface Issue {
  issueId: string
  issueNum: string
  summary: string
  typeCode?: string,
  issueTypeVO: IIssueType,
  parentIssueId?: string
  parentIssueNum?: string
  parentIssueSummary?: string
  relateIssueId?: string
  parentRelateSummary?: string
  sameParentIssueVOList?: Issue[],
  sameParentBugVOList?: Issue[]
  assigneeRealName: string
  reporterRealName: string
  statusVO: IStatus
  priorityVO: IPriority
  featureColor: string
  featureName: string
  epicColor: string
  epicName: string
  foundationFieldValue: {
    [key: string]: any
  }
}

export interface ILog {
  logId: string,
  field: string,
  fieldName: string,
  oldString: string,
  oldValue: any,
  newString: string,
  newValue: any,
  lastUpdateDate: string,
  email: string,
  lastUpdatedBy: string,
  name: string,
  loginName: string,
  realName: string,
  imageUrl: string,
  user: User,
  newStatus?: string,
  trigger?: string,
  removeResolution?: boolean,
  resolutionChanged?: boolean,
}
export type IReportContentType = 'chart' | 'text' | 'static_list' | 'dynamic_list'

export interface IBoard {
  boardId: string
  columnConstraint: string
  estimationStatistic: string
  name: string
  objectVersionNumber: number
  projectId: string
  swimlaneBasedCode: string
  userDefault: boolean
  userDefaultBoard: string
}
export type IIssueColumnName =
  'summary' |
  'issueNum' |
  'priority' |
  'sprint' |
  'reporter' |
  'creationDate' |
  'assign' |
  'status' |
  'lastUpdateDate' |
  'estimatedStartTime' |
  'estimatedEndTime' |
  'label' |
  'component' |
  'storyPoints' |
  'version' |
  'epic' |
  'feature';

export type IFieldType =
  'text' | 'input' | 'member' | 'single' | 'multiple' | 'radio' | 'checkbox' |
  'number' | 'time' | 'date' | 'datetime'

export type ISystemFieldCode =
  'summary' |
  'issueTypeId' |
  'issueNum' |
  'priorityId' |
  'sprint' |
  'reporterIds' |
  'createDate' |
  'assigneeId' |
  'status' |
  'lastUpdateDate' |
  'estimatedStartTime' |
  'estimatedEndTime' |
  'label' |
  'component' |
  'storyPoints' |
  'version' |
  'epic' |
  'feature';

export interface IFoundationHeader {
  title: string
  fieldType: string
  code: string
}
