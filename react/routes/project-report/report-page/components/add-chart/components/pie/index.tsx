import React, { useMemo, useCallback, useImperativeHandle } from 'react';
import Pie from '@/components/charts/pie-chart';
import PieSearch from '@/components/charts/pie-chart/search';
import usePieChartReport from '@/components/charts/pie-chart/usePieChartReport';
import { IReportChartBlock, PieSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId, getOrganizationId } from '@/utils/common';

import { ChartRefProps } from '../..';

export const transformPieSearch = (searchVO: PieSearchVO) => {
  if (!searchVO) {
    return undefined;
  }
  let chooseDimension: 'version' | 'sprint' | '' = '';
  let chooseId: string = '';
  if (searchVO.sprintId && !searchVO.versionId) {
    chooseDimension = 'sprint';
    chooseId = searchVO.sprintId;
  } else if (searchVO.versionId && !searchVO.sprintId) {
    chooseDimension = 'version';
    chooseId = searchVO.versionId;
  }
  return ({
    type: searchVO.fieldName,
    chooseDimension,
    chooseId,
  });
};
export interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
}
const PieComponent: React.FC<Props> = ({ innerRef, data }) => {
  const config = useMemo(() => transformPieSearch(data?.chartSearchVO as PieSearchVO), [data?.chartSearchVO]);
  const [searchProps, props] = usePieChartReport(config);
  const { type, chooseDimension, chooseId } = searchProps;
  const handleSubmit = useCallback(async (): Promise<PieSearchVO> => ({
    sprintId: chooseDimension === 'sprint' ? chooseId : undefined,
    versionId: chooseDimension === 'version' ? chooseId : undefined,
    projectId: getProjectId(),
    organizationId: getOrganizationId(),
    fieldName: type,
  }),
  [chooseDimension, chooseId, type]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <PieSearch {...searchProps} />
      <Pie {...props} />
    </div>
  );
};
export default PieComponent;
