import React, { useImperativeHandle, useCallback } from 'react';
import Sprint from '@/components/charts/sprint';
import SprintSearch from '@/components/charts/sprint/search';
import useSprintReport, { SprintConfig } from '@/components/charts/sprint/useSprintReport';
import { IReportChartBlock, SprintSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import { ChartRefProps } from '../..';
import { validateSearchDataBySearchProps } from '../../utils';

export const transformSprintSearch = (searchVO: SprintSearchVO | undefined): SprintConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return {
    restDayShow: searchVO.displayNonWorkingDay,
    sprintId: searchVO.sprintId,
    projectId: searchVO.projectId,
    useCurrentSprint: searchVO.currentSprint,
  };
};
export interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const SprintComponent: React.FC<Props> = ({ innerRef, projectId, data }) => {
  const [searchProps, props] = useSprintReport({
    ...transformSprintSearch(data?.chartSearchVO as SprintSearchVO),
    projectId,
    openValidate: true,
  });
  const handleSubmit = useCallback(async (): Promise<SprintSearchVO> => validateSearchDataBySearchProps(searchProps, ({
    displayNonWorkingDay: searchProps.restDayShow,
    sprintId: searchProps.sprintId,
    currentSprint: searchProps.useCurrentSprint,
    projectId: searchProps.projectId || getProjectId(),
  })),
  [searchProps]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <SprintSearch {...searchProps} />
      <Sprint {...props} />
    </div>
  );
};
export default SprintComponent;
