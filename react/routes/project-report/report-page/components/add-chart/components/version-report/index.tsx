import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import VersionReport from '@/components/charts/version-report';
import VersionReportSearch from '@/components/charts/version-report/search';
import useVersionReport, { VersionReportConfig } from '@/components/charts/version-report/useVersionReport';
import { getProjectId } from '@/utils/common';
import { IReportChartBlock, VersionReportSearchVO } from '@/routes/project-report/report-page/store';
import { ChartRefProps } from '../..';
import { validateSearchDataBySearchProps } from '../../utils';

export const transformVersionReportSearch = (searchVO: VersionReportSearchVO | undefined) : VersionReportConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    unit: searchVO.type,
    versionId: searchVO.versionId,
    projectId: searchVO.projectId,
  });
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const VersionReportComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformVersionReportSearch(data?.chartSearchVO as VersionReportSearchVO),
    projectId,
    openValidate: true,
  }), [data?.chartSearchVO, projectId]);
  const [props, searchProps] = useVersionReport(config);
  const { unit, versionId } = searchProps;
  const handleSubmit = useCallback(async (): Promise<VersionReportSearchVO> => validateSearchDataBySearchProps(searchProps, ({
    versionId,
    type: unit,
    projectId: searchProps.projectId || getProjectId(),
  })), [unit, versionId, searchProps]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <VersionReportSearch {...searchProps} />
      <VersionReport {...props} />
    </div>
  );
};

export default observer(VersionReportComponent);
