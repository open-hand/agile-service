import React, { memo } from 'react';
import { Spin } from 'choerodon-ui/pro';
import { EChartOption } from 'echarts';
import ReactEcharts from 'echarts-for-react';
import { useFontSize } from '@/components/charts/context';
import { IChartData, IChartType, IChartUnit } from './utils';

export interface ChartProps {
  loading: boolean,
  data: IChartData[] | null,
  chartType?: IChartType,
  type?: IChartUnit,
  option?: EChartOption
  chartCls?: string
}

const Chart: React.FC<ChartProps> = ({
  loading,
  option,
  data,
  chartCls,
}) => {
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const getOption = (): EChartOption => ({
    textStyle: {
      fontSize: FontSize,
    },
    ...(option || {}),
  });
  return (
    <div style={{ padding: '10px 20px 20px 10px' }} className={chartCls}>
      <Spin spinning={loading}>
        <ReactEcharts option={getOption()} key={`${JSON.stringify(data)}-${JSON.stringify(option)}`} />
      </Spin>
    </div>
  );
};

export default memo(Chart);
