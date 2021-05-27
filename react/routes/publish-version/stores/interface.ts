import {
  IPublishVersionTreeNode,
} from '@/api';
import { DetailContainerProps } from '@/components/detail-container';
import Record from 'choerodon-ui/pro/lib/data-set/Record';

export interface IPublishVersionBaseDetailEventsProps {
  load: (data: any) => any | Promise<any>
  createAfter: (data: any) => any /** 创建完发布版本事件 */
  delete: (data: any) => any
  selectIssue: (id: string) => any
  update: ((data: any, record: Record) => any | Promise<any>)
}
export interface IPublishVersionBaseDetailStore<T> {
  loading: boolean

  getCurrentData: T | undefined
  getDependencyList: Array<IPublishVersionTreeNode>
  getLoading: boolean
  getCurrentMenu: 'detail' | 'diff' | 'info' | string

  setDetailProps?: (data: DetailContainerProps) => void

  init: (events?: Partial<IPublishVersionBaseDetailEventsProps>, initData?: { detailProps?: DetailContainerProps }, ...other: any) => void
  clear: Function
  loadData: Function
  findAppServiceByCode: (code: string, data: any) => any
  select: (data: T | any) => void
  setCurrentMenu: (data: 'detail' | 'diff' | 'info' | string) => void
}
