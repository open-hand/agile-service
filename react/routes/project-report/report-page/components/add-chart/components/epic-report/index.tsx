import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import EpicReport from '@/components/charts/epic-report';
import EpicReportSearch from '@/components/charts/epic-report/search';
import useEpicReport, { EpicReportConfig } from '@/components/charts/epic-report/useEpicReport';
import { getProjectId } from '@/utils/common';
import { IReportChartBlock, EpicReportSearchVO } from '@/routes/project-report/report-page/store';
import { ChartRefProps } from '../..';
import { validateSearchDataBySearchProps } from '../../utils';

export const transformEpicReportSearch = (searchVO: EpicReportSearchVO | undefined): EpicReportConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    unit: searchVO.type,
    epicId: searchVO.epicId,
    projectId: searchVO.projectId,
  });
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const EpicReportComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformEpicReportSearch(data?.chartSearchVO as EpicReportSearchVO),
    projectId,
    openValidate: true,
  }), [data?.chartSearchVO, projectId]);
  const [props, searchProps] = useEpicReport(config);
  const { unit, epicId } = searchProps;
  const handleSubmit = useCallback(async (): Promise<EpicReportSearchVO> => validateSearchDataBySearchProps(searchProps, ({
    epicId,
    type: unit,
    projectId: searchProps.projectId || getProjectId(),
  })),
  [searchProps, epicId, unit]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <EpicReportSearch {...searchProps} />
      <EpicReport {...props} />
    </div>
  );
};

export default observer(EpicReportComponent);
