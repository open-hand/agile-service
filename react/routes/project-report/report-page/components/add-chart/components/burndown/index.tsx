import React, {
  useCallback, useImperativeHandle, useMemo, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { stores } from '@choerodon/boot';
import BurnDown from '@/components/charts/burn-down';
import BurnDownSearch from '@/components/charts/burn-down/search';
import useBurnDownReport, { BurnDownConfig } from '@/components/charts/burn-down/useBurnDownReport';
import { IReportChartBlock, BurnDownSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import { ChartRefProps } from '../..';

const { AppState } = stores;

export const transformBurnDownSearch = (searchVO: BurnDownSearchVO | undefined): BurnDownConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return {
    type: searchVO.type,
    restDayShow: searchVO.displayNonWorkingDay,
    sprintId: searchVO.sprintId,
    quickFilter: {
      onlyStory: searchVO.onlyStory,
      onlyMe: searchVO.assigneeId === AppState.userInfo.id,
      quickFilters: searchVO.quickFilterIds,
      personalFilters: searchVO.personalFilterIds,
    },
    projectId: searchVO.projectId,
    useCurrentSprint: searchVO.currentSprint,
    searchVO: searchVO.currentSearchVO,
  };
};
export interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  projectId?: string
  data?: IReportChartBlock
}
const BurnDownComponent: React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformBurnDownSearch(data?.chartSearchVO as BurnDownSearchVO),
    projectId,
    openValidate: true,
  }),
  [data?.chartSearchVO, projectId]);
  const [searchProps, props] = useBurnDownReport(config);
  const handleSubmit = useCallback(async (): Promise<BurnDownSearchVO | false> => {
    const searchDataSet = searchProps.searchDataSet!;
    return (await searchDataSet.validate() ? ({
      type: searchProps.type,
      displayNonWorkingDay: searchProps.restDayShow,
      projectId: searchProps.projectId || getProjectId(),
      sprintId: searchProps.sprintId,
      currentSprint: searchProps.useCurrentSprint,
      quickFilterIds: searchProps.quickFilter.quickFilters,
      onlyStory: searchProps.quickFilter.onlyStory,
      assigneeId: searchProps.quickFilter.onlyMe ? AppState.userInfo.id : undefined,
      personalFilterIds: searchProps.quickFilter.personalFilters,
      currentSearchVO: searchProps.searchVO,
    }) : false);
  },
  [searchProps]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <BurnDownSearch {...searchProps} />
      <BurnDown {...props} />
    </div>
  );
};
export default observer(BurnDownComponent);
