import React, { useMemo, memo } from 'react';
import { Spin } from 'choerodon-ui';
import { EChartOption } from 'echarts';
import ReactEcharts from 'echarts-for-react';
import { map } from 'lodash';
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
}) => {
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const getOption = (): EChartOption => ({
    textStyle: {
      fontSize: FontSize,
    },
    // grid: {
    //   top: 60,
    //   bottom: 30,
    //   left: 0,
    //   right: 40,
    //   containLabel: true,
    // },
    ...(option || {}),
  });
  return (
    <div style={{ padding: '0 20px 20px 10px' }}>
      <Spin spinning={loading}>
        <ReactEcharts option={getOption()} />
      </Spin>
    </div>
  );
};

export default memo(Chart);
