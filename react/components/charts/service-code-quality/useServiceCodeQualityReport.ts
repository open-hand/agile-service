import {
  useState, useEffect, useCallback, useMemo,
} from 'react';
import { rdqamApi } from '@/api';
import { getProjectId } from '@/utils/common';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import moment, { Moment } from 'moment';
import { ServiceCodeQualityProps, ServiceCodeQualityData } from './index';
import { ServiceCodeQualitySearchProps, ServiceCodeQualityType } from './search';

export interface ServiceCodeQualityConfig {
  projectId?: string
  serviceId?: string
  days?: number
  range?: [Moment, Moment] | null
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
  const [days, setDays] = useControlledDefaultValue<number | null>(config?.days ?? 7);
  const [range, setRange] = useControlledDefaultValue<[Moment, Moment] | undefined | null>(config?.range);
  const [serviceId, setServiceId] = useControlledDefaultValue(config?.serviceId ?? '');
  const [type, setType] = useControlledDefaultValue<ServiceCodeQualityType>(config?.type ?? 'issue');
  const { startDate, endDate } = useMemo(() => ({
    startDate: `${(range ? range[0] : moment().subtract(days, 'days')).format('YYYY-MM-DD')} 00:00:00`,
    endDate: `${(range ? range[1] : moment().add(1, 'days')).format('YYYY-MM-DD')} 00:00:00`,
  }), [days, range]);
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
  const searchProps: ServiceCodeQualitySearchProps = {
    projectId,
    days,
    setDays: (value) => {
      setDays(value);
      setRange(null);
    },
    range,
    onRangeChange: (value) => {
      if (Array.isArray(value)) {
        if (!value[0] || !value[1]) {
          return;
        }
      }
      setDays(null);
      setRange(value);
    },
    serviceId,
    setServiceId,
    onEmpty: handleEmpty,
    type,
    setType,
  };
  return [props, searchProps];
};

export default useServiceCodeQualityReport;
