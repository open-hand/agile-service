import { ModalProps } from 'choerodon-ui/lib/modal';

export interface User {
  email: string
  enabled?: boolean
  id: number
  imageUrl: string | null
  ldap: boolean
  loginName: string
  realName: string
}

export interface Issue {
  issueId: string
  summary: string
}

export interface PI {
  code: string
  name: string
}

export interface IStatus {
  id: string
  valueCode: 'todo' | 'doing' | 'done' | 'prepare'
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
  handleOk: (promise: ()=>Promise<boolean>) => Promise<void>,
  handleCancel: (promise: ()=>Promise<boolean>) => Promise<void>,
  close: (destroy?: boolean) => void,
  update: (modalProps:ModalProps) => void
}
export interface IIssueType{
  colour: string,
  description: string,
  icon: string,
  id: string,
  name: string,
  stateMachineId: string,
  typeCode: string,
}
