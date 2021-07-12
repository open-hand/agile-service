import { customReportApi } from '@/api';
import getOptions from '@/routes/ReportHost/custom-report/components/Chart/utils';

import { IChartRes } from '@/routes/ReportHost/custom-report/Report/Report';
// @ts-ignore
import JSONbig from 'json-bigint';
import { isEmpty } from 'lodash';

const JSONbigString = JSONbig({ storeAsString: true });

interface ICustomChartConfigLayout {
  h: number
  i: string
  minH: number
  minW: number
  w: number
  x: number
  y: number
}

interface ICustomChartConfig {
  layout: ICustomChartConfigLayout
  name: string
  type: string
  groupId: 'agile'
  title: string,
  customData: { extendSearchVO?: any } & Partial<Pick<IChartRes, 'customChartData'>> & Omit<IChartRes, 'customChartData'>
  describe: string
  img: any
}

export async function loadCustomReportData(customChartConfig: ICustomChartConfig) {
  const { customData } = customChartConfig;

  const config = {
    chartType: customData.chartType,
    statisticsType: customData.statisticsType,
    analysisField: customData.analysisField,
    comparedField: customData.comparedField,
    analysisFieldPredefined: !['pro', 'org'].includes(String(customData.analysisField).split('_')[0]), //
    comparedFieldPredefined: !['pro', 'org'].includes(String(customData.comparedField).split('_')[0]),
    searchVO: isEmpty(customData.searchJson) ? undefined : JSONbigString.parse(customData.searchJson),
    extendSearchVO: customData.extendSearchVO,
  };
  const res = await customReportApi.getData(config);
  const data = (res.dimensionList || []).map((item: any) => ({ ...item, pointList: item.pointList.map((point: any) => ({ ...point, value: parseFloat(point.value.toString()) })) }));
  const newOptions = isEmpty(data) ? undefined : getOptions(customData.chartType, customData.statisticsType, data, 12);
  return newOptions;
}
