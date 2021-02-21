import {
  observable, action,
} from 'mobx';
import {
  IReportContentType, User, IIssueColumnName, ISearchVO,
} from '@/common/types';
import { IBurndownChartType } from '@/components/charts/burn-down';
import { IPieChartType } from '@/components/charts/pie-chart';
import { IUnit } from '@/components/charts/version-report/search';
import { ServiceCodeQualityType } from '@/components/charts/service-code-quality/search';

export type IChartCode = 'burn_down_report' | 'sprint_report' | 'cumulative_flow_diagram'
  | 'pie_chart' | 'version_chart' | 'epic_chart' | 'version_burn_down_report'
  | 'epic_burn_down_report' | 'velocity_chart' | 'code_quality' | 'code_quality_vary'
  | 'service_code_quality'

const reorder = <T>(list: T[], startIndex: number, endIndex: number): T[] => {
  const result = Array.from(list);
  const [removed] = result.splice(startIndex, 1);
  result.splice(endIndex, 0, removed);

  return result;
};

interface IBaseReportBlock {
  key: string
  title: string
  type: IReportContentType
}
export type BurnDownSearchVO = {
  projectId: string
  type?: IBurndownChartType
  sprintId?: string
  currentSprint?: boolean
  displayNonWorkingDay: boolean
  onlyStory: boolean
  assigneeId?: string
  quickFilterIds: string[]
  personalFilterIds: string[]
  currentSearchVO?: ISearchVO
}
export type SprintSearchVO = {
  projectId: string
  sprintId?: string
  currentSprint?: boolean
  displayNonWorkingDay: boolean
}
export type AccumulationSearchVO = {
  projectId: string
  boardId: string
  startDate: string
  endDate: string
  quickFilterIds?: string[]
}

export type PieSearchVO = {
  sprintId?: string
  versionId?: string
  statusId?: string
  projectId: string,
  organizationId: string,
  fieldName: IPieChartType,
}

export type VersionReportSearchVO = {
  versionId: string
  type: IUnit,
  projectId: string,
}

export type EpicReportSearchVO = {
  epicId: string
  type: IUnit,
  projectId: string,
}

export type VersionBurndownSearchVO = {
  type: 'version',
  versionId: string,
  calibrationSprint: boolean,
  projectId: string,
}

export type EpicBurndownSearchVO = {
  type: 'epic',
  epicId: string,
  calibrationSprint: boolean,
  projectId: string,
}

export type IterationSpeedSearchVO = {
  type: IUnit,
  projectId: string,
}
export type CodeQualitySearchVO = {
  projectId: string,
}
export type CodeQualityVarySearchVO = {
  projectId: string,
  days: number
}
export type ServiceCodeQualitySearchVO = {
  projectId: string,
  days: number | null
  type: ServiceCodeQualityType
  serviceId: string
  startDate?: string
  endDate?: string
}

export type ChartSearchVO = BurnDownSearchVO | SprintSearchVO
  | AccumulationSearchVO | PieSearchVO | VersionReportSearchVO
  | EpicReportSearchVO | VersionBurndownSearchVO | EpicBurndownSearchVO
  | IterationSpeedSearchVO | CodeQualitySearchVO | CodeQualityVarySearchVO
  | ServiceCodeQualitySearchVO

export interface IReportChartBlock extends IBaseReportBlock {
  type: 'chart'
  chartCode: IChartCode
  chartSearchVO: ChartSearchVO
}
export interface SearchVO {
  advancedSearchArgs?: {
    issueTypeId?: string[],
    reporterIds?: string[],
    statusId?: string[],
    priorityId?: string[],
  },
  otherArgs?: {
    assigneeId?: string[],
    issueIds?: string[],
    component?: string[],
    epic?: string[],
    feature?: string[],
    label?: string[],
    sprint?: string[],
    summary?: string[],
    version?: string[],
    withChildren?: boolean
  },
  searchArgs?: {
    createStartDate?: string,
    createEndDate?: string,
    updateStartDate?: string,
    updateEndDate?: string,
  },
  quickFilterIds?: string[],
  contents?: string[],
}
export interface IReportListBlock extends IBaseReportBlock {
  type: 'static_list' | 'dynamic_list'
  colList: IIssueColumnName[]
  searchVO: SearchVO
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
  setReportData(reportData: IProjectReport) {
    this.blockList = (reportData.reportUnitList || []).map((block) => ({ ...block, key: String(Math.random()) }));
    this.baseInfo = reportData;
  }

  @action('设置objectVersionNumber')
  setObjectVersionNumber(objectVersionNumber: number) {
    if (this.baseInfo) {
      this.baseInfo.objectVersionNumber = objectVersionNumber;
    }
  }

  @action('block排序')
  sortBlock(sourceIndex: number, destinationIndex: number) {
    this.blockList = reorder(
      this.blockList,
      sourceIndex,
      destinationIndex,
    );
  }
}

export default ProjectReportStore;
