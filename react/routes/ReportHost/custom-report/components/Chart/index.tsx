import React, { memo } from 'react';
import { Spin } from 'choerodon-ui';
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
}

const Chart: React.FC<ChartProps> = ({
  loading,
  option,
  data,
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
    <div style={{ padding: '10px 20px 5px 10px' }}>
      <Spin spinning={loading}>
        <ReactEcharts option={getOption()} key={JSON.stringify(data)} />
      </Spin>
    </div>
  );
};

export default memo(Chart);
