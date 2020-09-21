import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import VersionReport from '@/components/charts/version-report';
import VersionReportSearch from '@/components/charts/version-report/search';
import useVersionReport from '@/components/charts/version-report/useVersionReport';
import { getProjectId } from '@/utils/common';
import { IReportChartBlock, VersionReportSearchVO } from '@/routes/project-report/report-page/store';
import { ChartRefProps } from '../..';

export const transformVersionReportSearch = (searchVO: VersionReportSearchVO) => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    unit: searchVO.type,
    versionId: searchVO.versionId,
  });
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
}
const VersionReportComponent:React.FC<Props> = ({ innerRef, data }) => {
  const config = useMemo(() => transformVersionReportSearch(data?.chartSearchVO as VersionReportSearchVO), [data?.chartSearchVO]);
  const [props, searchProps] = useVersionReport(config);
  const { unit, versionId } = searchProps;
  const handleSubmit = useCallback(async (): Promise<VersionReportSearchVO> => ({
    versionId,
    type: unit,
    projectId: getProjectId(),
  }),
  [unit, versionId]);

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
