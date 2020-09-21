import React, { useCallback, useImperativeHandle, useMemo } from 'react';
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
  };
};
export interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
}
const BurnDownComponent: React.FC<Props> = ({ innerRef, data }) => {
  const config = useMemo(() => transformBurnDownSearch(data?.chartSearchVO as BurnDownSearchVO), [data?.chartSearchVO]);
  const [searchProps, props] = useBurnDownReport(config);
  const handleSubmit = useCallback(async (): Promise<BurnDownSearchVO> => ({
    type: searchProps.type,
    displayNonWorkingDay: searchProps.restDayShow,
    sprintId: searchProps.sprintId,
    quickFilterIds: searchProps.quickFilter.quickFilters,
    onlyMe: searchProps.quickFilter.onlyMe,
    onlyStory: searchProps.quickFilter.onlyStory,
    assigneeId: searchProps.quickFilter.onlyMe ? AppState.userInfo.id : undefined,
    personalFilterIds: searchProps.quickFilter.personalFilters,
    projectId: getProjectId(),
  }),
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
export default BurnDownComponent;
