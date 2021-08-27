import React, {
  useCallback, useImperativeHandle, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
// @ts-ignore
import JSONbig from 'json-bigint';
import { isEmpty } from 'lodash';
import useReport, { ChartConfig } from '@/routes/ReportHost/custom-report/components/Chart/useReport';
import { IReportChartBlock, CustomReportSearchVO } from '@/routes/project-report/report-page/store';
import { IField } from '@/common/types';
import { ChartRefProps } from '../..';
import {
  ICreateData,
} from '@/api';
import ChartSearch from '@/routes/ReportHost/custom-report/components/ChartSearch';
import Chart from '@/routes/ReportHost/custom-report/components/Chart';
import { getProjectId } from '@/utils/common';
import useDimension
  from '@/routes/project-report/report-page/components/add-chart/components/custom-report/useDimension';

import './index.less';

const JSONbigString = JSONbig({ storeAsString: true });

export interface CustomChartDataProps extends ICreateData{
  id: string,
}

export interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  projectId?: string
  data?: IReportChartBlock
  customChartData?: CustomChartDataProps,
}

export const transformCustomReportSearch = (searchVO: CustomReportSearchVO | undefined, dimension?: IField[]): ChartConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  const analysisField = searchVO?.analysisField;
  const comparedField = searchVO?.comparedField;
  return {
    chartType: searchVO?.chartType,
    statisticsType: searchVO?.statisticsType,
    analysisField,
    comparedField,
    // @ts-ignore
    analysisFieldPredefined: !dimension ? searchVO.analysisFieldPredefined : (analysisField && dimension.find((item) => item.code === analysisField)?.system),
    // @ts-ignore
    comparedFieldPredefined: !dimension ? searchVO.comparedFieldPredefined : comparedField && dimension.find((item) => item.code === comparedField)?.system,
    searchVO: searchVO?.searchVO || (isEmpty(searchVO.searchJson) ? undefined : JSONbigString.parse(searchVO.searchJson)),
    chartSearchVO: searchVO?.currentSearchVO,
  };
};

const CustomReportComponent: React.FC<Props> = ({
  innerRef, projectId, data, customChartData,
}) => {
  const prefixCls = useMemo(() => 'c7nag-project-custom-report', []);
  const dimension = useDimension();

  const config = useMemo(() => ({
    ...transformCustomReportSearch(
      // @ts-ignore
      !data?.chartSearchVO?.chartType && customChartData ? customChartData : data?.chartSearchVO as CustomReportSearchVO,
      dimension,
    ),
    projectId,
  }), [customChartData, data?.chartSearchVO, projectId, dimension]);

  // @ts-ignore
  const [searchProps, props] = useReport(config, 18);

  const handleSubmit = useCallback(async (): Promise<CustomReportSearchVO> => ({
    chartType: config?.chartType,
    statisticsType: config?.statisticsType,
    analysisField: config?.analysisField,
    comparedField: config?.comparedField,
    analysisFieldPredefined: config?.analysisFieldPredefined,
    comparedFieldPredefined: config?.comparedFieldPredefined,
    searchVO: config.searchVO,
    currentSearchVO: searchProps.searchVO,
    projectId: searchProps.projectId || getProjectId(),
  }),
  [searchProps]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <ChartSearch {...searchProps} searchCls={`${prefixCls}-search`} />
      <Chart {...props} chartCls={`${prefixCls}-chart`} />
    </div>
  );
};
export default observer(CustomReportComponent);
