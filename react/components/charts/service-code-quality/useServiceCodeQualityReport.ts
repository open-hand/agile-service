import {
  useState, useEffect, useCallback, useMemo,
} from 'react';
import { rdqamApi } from '@/api';
import { getProjectId } from '@/utils/common';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import moment from 'moment';
import { ServiceCodeQualityProps, ServiceCodeQualityData } from './index';
import { ServiceCodeQualitySearchProps, ServiceCodeQualityType } from './search';

export interface ServiceCodeQualityConfig {
  projectId?: string
  serviceId?: string
  days?: number
  type?: ServiceCodeQualityType
}

const useServiceCodeQualityReport = (config?: ServiceCodeQualityConfig, onFinish?: Function): [ServiceCodeQualityProps, ServiceCodeQualitySearchProps] => {
  const projectId = config?.projectId || getProjectId();
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<ServiceCodeQualityData>({
    legend: [],
    dates: [],
    duplicatedLines: [],
    duplicatedLinesRate: [],
    nclocs: [],
  });
  const [days, setDays] = useControlledDefaultValue(config?.days ?? 7);
  const [serviceId, setServiceId] = useControlledDefaultValue(config?.serviceId ?? '');
  const [type, setType] = useControlledDefaultValue<ServiceCodeQualityType>(config?.type ?? 'issue');
  const { startDate, endDate } = useMemo(() => ({
    startDate: `${moment().subtract(days, 'days').format('YYYY-MM-DD')} 00:00:00`,
    endDate: `${moment().add(1, 'days').format('YYYY-MM-DD')} 00:00:00`,
  }), [days]);
  const handleEmpty = useCallback(() => {
    onFinish && setTimeout(onFinish);
  }, [onFinish]);
  const loadChartData = useCallback(() => {
    setLoading(true);
    if (serviceId) {
      rdqamApi.project(projectId).loadByService({
        serviceId,
        startDate,
        endDate,
        type,
      }).then((res: ServiceCodeQualityData) => {
        setData(res);
        setLoading(false);
        onFinish && setTimeout(onFinish);
      });
    }
  }, [serviceId, projectId, startDate, endDate, type, onFinish]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const props = {
    loading,
    data,
    type,
  };
  const searchProps = {
    projectId,
    days,
    setDays,
    serviceId,
    setServiceId,
    onEmpty: handleEmpty,
    type,
    setType,
  };
  return [props, searchProps];
};

export default useServiceCodeQualityReport;
