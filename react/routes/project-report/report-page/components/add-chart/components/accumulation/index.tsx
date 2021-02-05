import React, { useCallback, useImperativeHandle, useMemo } from 'react';
import Accumulation from '@/components/charts/accumulation';
import AccumulationSearch from '@/components/charts/accumulation/search';
import useAccumulationReport, { AccumulationConfig } from '@/components/charts/accumulation/useAccumulationReport';
import { IReportChartBlock, AccumulationSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import moment from 'moment';
import { ChartRefProps } from '../..';

export const transformAccumulationSearch = (searchVO: AccumulationSearchVO | undefined): AccumulationConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return {
    boardId: searchVO.boardId,
    quickFilterIds: searchVO.quickFilterIds,
    range: [moment(searchVO.startDate), moment(searchVO.endDate)],
    projectId: searchVO.projectId,
  };
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const AccumulationComponent: React.FC<Props> = ({ innerRef, data, projectId }) => {
  const config = useMemo(() => ({
    ...transformAccumulationSearch(data?.chartSearchVO as AccumulationSearchVO),
    projectId,
  }),
  [data?.chartSearchVO, projectId]);
  const [searchProps, props] = useAccumulationReport(config);
  const handleSubmit = useCallback(async (): Promise<AccumulationSearchVO> => ({
    boardId: searchProps.boardId,
    startDate: searchProps.range[0].format('YYYY-MM-DD 00:00:00'),
    endDate: `${searchProps.range[1].format('YYYY-MM-DD')} 23:59:59`,
    quickFilterIds: searchProps.quickFilterIds,
    projectId: searchProps.projectId || getProjectId(),
  }),
  [searchProps]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <AccumulationSearch {...searchProps} />
      <Accumulation {...props} />
    </div>
  );
};
export default AccumulationComponent;
