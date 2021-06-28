import React, { useMemo, memo } from 'react';
import { Spin } from 'choerodon-ui';
import { EChartOption } from 'echarts';
import ReactEcharts from 'echarts-for-react';
import { map } from 'lodash';
import { useFontSize } from '@/components/charts/context';
import { IIssueFilterFormProps } from '@/components/issue-filter-form';
import { IChoseFieldComponentProps } from '@/components/chose-field';
import { ISearchVO } from '@/common/types';
import ChoseFieldStore from '@/components/chose-field/store';
import { IChosenFieldField } from '@/components/chose-field/types';
import { IIssueFilterFormDataProps } from '@/components/issue-filter-form/IssueFilterForm';
import { IChartData, IChartType, IChartUnit } from './utils';

export interface ChartProps {
  loading: boolean,
  data: IChartData[] | null,
  chartType?: IChartType,
  type?: IChartUnit,
  option?: EChartOption
  searchVO?: ISearchVO,
  choseFieldStore: ChoseFieldStore,
  choseComponentProps: IChoseFieldComponentProps,
  filterComponentProps: IIssueFilterFormProps,
  fields: IChosenFieldField[],
  filterData: IIssueFilterFormDataProps,
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
