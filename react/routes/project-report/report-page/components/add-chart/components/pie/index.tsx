import React, { useMemo, useCallback, useImperativeHandle } from 'react';
import Pie from '@/components/charts/pie-chart';
import PieSearch from '@/components/charts/pie-chart/search';
import usePieChartReport, { PieConfig } from '@/components/charts/pie-chart/usePieChartReport';
import { IReportChartBlock, PieSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId, getOrganizationId } from '@/utils/common';

import { ChartRefProps } from '../..';

export const transformPieSearch = (searchVO: PieSearchVO | undefined): PieConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  let chooseDimension: 'version' | 'sprint' | 'status' | '' = '';
  let chooseId: string = '';
  if (searchVO.sprintId) {
    chooseDimension = 'sprint';
    chooseId = searchVO.sprintId;
  } else if (searchVO.versionId) {
    chooseDimension = 'version';
    chooseId = searchVO.versionId;
  } else if (searchVO.statusId) {
    chooseDimension = 'status';
    chooseId = searchVO.statusId;
  }
  return ({
    type: searchVO.fieldName,
    chooseDimension,
    chooseId,
    projectId: searchVO.projectId,
  });
};
export interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const PieComponent: React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformPieSearch(data?.chartSearchVO as PieSearchVO),
    projectId,
  }), [data?.chartSearchVO, projectId]);
  const [searchProps, props] = usePieChartReport(config);
  const { type, chooseDimension, chooseId } = searchProps;
  const handleSubmit = useCallback(async (): Promise<PieSearchVO> => ({
    sprintId: chooseDimension === 'sprint' ? chooseId : undefined,
    versionId: chooseDimension === 'version' ? chooseId : undefined,
    statusId: chooseDimension === 'status' ? chooseId : undefined,
    projectId: searchProps.projectId || getProjectId(),
    organizationId: getOrganizationId(),
    fieldName: type,
  }),
  [chooseDimension, chooseId, searchProps.projectId, type]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <PieSearch {...searchProps} />
      <Pie {...props} link={false} />
    </div>
  );
};
export default PieComponent;
