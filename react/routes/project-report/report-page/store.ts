import {
  observable, action, toJS,
} from 'mobx';
import { remove, findIndex } from 'lodash';
import { IReportContentType } from '@/common/types';
import { SprintConfig } from '@/components/charts/sprint/useSprintReport';
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
export interface IReportChartBlock extends IBaseReportBlock {
  type: 'chart'
  chartCode: IChartCode
  chartSearchVO: BurnDownSearchVO | SprintConfig
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
class ProjectReportStore {
  @observable blockList: IReportBlock[] = []

  @observable baseInfo: { id: string } | null = null

  @action('添加一个block')
  addBlock(block: IReportBlock) {
    this.blockList.push(block);
  }

  @action('更新一个block')
  updateBlock(index: number, block: IReportBlock) {
    console.log(index);
    this.blockList[index] = block;
    console.log(toJS(this.blockList));
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
