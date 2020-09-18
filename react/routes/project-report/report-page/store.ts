import {
  observable, action,
} from 'mobx';
import { IReportContentType, User } from '@/common/types';
import { IBurndownChartType } from '@/components/charts/burn-down';

export type IChartCode = 'burn_down_report' | 'sprint_report'

interface IBaseReportBlock {
  title: string
  type: IReportContentType
}
export type BurnDownSearchVO = {
  projectId: string
  type?: IBurndownChartType
  sprintId?: string
  displayNonWorkingDay: boolean
  onlyMe: boolean
  onlyStory: boolean
  assigneeId?: string
  quickFilterIds: string[]
  personalFilterIds: string[]
}
export type SprintSearchVO = {
  projectId: string
  sprintId?: string
  displayNonWorkingDay: boolean
}
export type ChartSearchVO = BurnDownSearchVO | SprintSearchVO
export interface IReportChartBlock extends IBaseReportBlock {
  type: 'chart'
  chartCode: IChartCode
  chartSearchVO: ChartSearchVO
}
export interface IReportListBlock extends IBaseReportBlock {
  type: 'list'
  data: {
    filter: any
  }
}
export interface IReportTextBlock extends IBaseReportBlock {
  type: 'text'
  content: string
}
export type IReportBlock = IReportTextBlock | IReportListBlock | IReportChartBlock

export interface IProjectReport {
  id: string
  title: string
  description?: string
  ccList: User[]
  receiverList: User[]
  reportUnitList: IReportBlock[]
  objectVersionNumber: number
}
class ProjectReportStore {
  @observable blockList: IReportBlock[] = []

  @observable baseInfo: IProjectReport | null = null

  @action('添加一个block')
  addBlock(block: IReportBlock) {
    this.blockList.push(block);
  }

  @action('更新一个block')
  updateBlock(index: number, block: IReportBlock) {
    this.blockList[index] = block;
  }

  @action('移除一个block')
  removeBlock(index: number) {
    this.blockList.splice(index, 1);
  }

  @action('设置ReportData')
  setReportData(reportData: any) {
    this.blockList = reportData.reportUnitList;
    this.baseInfo = reportData;
  }
}

export default ProjectReportStore;
