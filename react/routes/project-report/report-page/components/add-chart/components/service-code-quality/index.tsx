import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import ServiceCodeQualityReport from '@/components/charts/service-code-quality';
import ServiceCodeQualitySearch from '@/components/charts/service-code-quality/search';
import { getProjectId } from '@/utils/common';
import { ServiceCodeQualitySearchVO, IReportChartBlock } from '@/routes/project-report/report-page/store';
import useServiceCodeQualityReport, { ServiceCodeQualityConfig } from '@/components/charts/service-code-quality/useServiceCodeQualityReport';
import moment from 'moment';
import { ChartRefProps } from '../..';

export const transformServiceCodeQualitySearch = (searchVO: ServiceCodeQualitySearchVO | undefined): ServiceCodeQualityConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return {
    projectId: searchVO.projectId,
    days: searchVO.days as number,
    type: searchVO.type,
    serviceId: searchVO.serviceId,
    range: searchVO.startDate && searchVO.endDate ? [moment(searchVO.startDate), moment(searchVO.endDate)] : undefined,
  };
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const ServiceCodeQualityReportComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformServiceCodeQualitySearch(data?.chartSearchVO as ServiceCodeQualitySearchVO),
    projectId,
  }), [data?.chartSearchVO, projectId]);
  const [props, searchProps] = useServiceCodeQualityReport(config);
  const handleSubmit = useCallback(async (): Promise<ServiceCodeQualitySearchVO> => ({
    projectId: searchProps.projectId || getProjectId(),
    days: searchProps.days,
    type: searchProps.type,
    serviceId: searchProps.serviceId,
    startDate: searchProps?.range ? searchProps?.range[0].format('YYYY-MM-DD 00:00:00') : undefined,
    endDate: searchProps?.range ? searchProps?.range[1].format('YYYY-MM-DD 00:00:00') : undefined,
  }),
  [searchProps.days, searchProps.projectId, searchProps?.range, searchProps.serviceId, searchProps.type]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <ServiceCodeQualitySearch {...searchProps} />
      <ServiceCodeQualityReport {...props} />
    </div>
  );
};

export default observer(ServiceCodeQualityReportComponent);
