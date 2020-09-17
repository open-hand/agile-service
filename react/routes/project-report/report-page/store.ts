import {
  observable, action,
} from 'mobx';
import { remove, findIndex } from 'lodash';
import { IReportContentType } from '@/common/types';
import { BurnDownConfig } from '@/components/charts/burn-down/useBurnDownReport';
import { SprintConfig } from '@/components/charts/sprint/useSprintReport';

export type IChartType = 'burndown' | 'sprint'

interface IBaseReportBlock {
  id: string
  title: string
  type: IReportContentType
}

export interface IReportChartBlock extends IBaseReportBlock {
  type: 'chart'
  chartType: IChartType
  data: {
    filter: BurnDownConfig | SprintConfig
  }
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

  @observable baseInfo: Object | null = null

  @action('添加一个block')
  addBlock(block: IReportBlock) {
    this.blockList.push(block);
  }

  @action('更新一个block')
  updateBlock(block: IReportBlock) {
    const { id } = block;
    const index = findIndex(this.blockList, { id });
    this.blockList[index] = block;
  }

  @action('移除一个block')
  removeBlock(id: string) {
    remove(this.blockList, (block) => block.id === id);
  }

  @action('设置ReportData')
  setReportData(reportData: any) {
    this.blockList = reportData.reportUnitList;
    this.baseInfo = reportData;
  }
}

export default ProjectReportStore;
