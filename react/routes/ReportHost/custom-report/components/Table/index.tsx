import React, {
  useCallback, useMemo,
} from 'react';
import {
  DataSet, Table, Tooltip,
} from 'choerodon-ui/pro';
import { runInAction, toJS } from 'mobx';
import {
  find, isEmpty, merge, mergeWith, pick,
} from 'lodash';
import { IField } from '@/common/types';
import { IChartData } from '../Chart/utils';
import { ChartProps } from '../Chart';

interface TableFilterData {
  params?: string
  analysisValue?: string[]
}
interface CustomReportTableProps extends ChartProps {
  dimension: IField[]
  analysisField?: string
}
type IChartDataPointListItem = IChartData['pointList'][0]
interface TableDataItem extends IChartDataPointListItem {
  total?: any
  [key: string]: any
}
const { Column } = Table;
const CustomReportTable: React.FC<CustomReportTableProps> = ({
  data, dimension, analysisField, type, chartType, ...otherProps
}) => {
  const tableData = useMemo(() => {
    const newData = toJS(data)
      ?.reduce((preValue, currentValue) => {
        const newValue = currentValue.pointList.reduce((pre, v) => ({ ...pre, [`analysisId-${v.analysisId}-${v.analysisValue}`]: { ...v, total: v.value || 0, [`value-${currentValue.comparedId}`]: v.value } }), {} as any);

        return mergeWith(preValue, newValue, (aValue: any, bValue: any, key) => {
          if (key === 'total') {
            return (aValue || 0) + (bValue || 0);
          }
          return undefined;
        });
      }, {} as any);
    return Object.values<TableDataItem>(newData);
  },
  [data]);
  const field = useMemo(() => find(dimension, { code: analysisField }), [analysisField, dimension]);
  // const fields = useMemo(() => {}, []);

  const queryDataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'analysisValue',
      label: field?.name,
      multiple: true,
      options: new DataSet({
        paging: false,
        data: tableData.map((i) => ({
          meaning: i.analysisValue,
          value: i.analysisId,
        })),
      }),
    }],
    events: {
      update: ({ value, name, record }: any) => {
        loadData();
      },
    },
  }), [field?.name, tableData]);
  const dataSet = useMemo(() => new DataSet({
    selection: false,
    autoCreate: false,
    autoQuery: false,
    paging: true,
    data: tableData,
    queryDataSet,
    fields: [{ name: 'analysisValue', label: field?.name }],
  }), [tableData, queryDataSet, field?.name]);

  const handleFilterData = (originData: TableDataItem[], filters?: TableFilterData) => {
    const pickProps = pick(filters || {}, ['analysisValue', 'params']);
    if (!Object.keys(pickProps).length) {
      return originData;
    }
    const analysisIds = pickProps.analysisValue;
    const { params } = pickProps;
    return originData.filter((item) => {
      const res = isEmpty(analysisIds) || analysisIds?.includes(item.analysisId);
      return res && (isEmpty(params) || String(item.analysisValue).toLowerCase().indexOf(String(params).toLowerCase()) !== -1);
    });
  };

  const loadData = useCallback((page: number = 1, pageSize: number = dataSet.pageSize, filters?: TableFilterData) => {
    const startPos = (page - 1) * pageSize;
    const filterData = filters || queryDataSet.current?.toJSONData();
    const filterTableData = handleFilterData(tableData, filterData);
    runInAction(() => {
      dataSet.loadData(filterTableData.slice(startPos, startPos + pageSize), filterTableData.length);
      dataSet.currentPage = page;
      dataSet.pageSize = pageSize;
    });
  }, [dataSet, queryDataSet, tableData]);

  const renderColumns = useCallback(() => {
    const columns = [<Column name="value" header={type === 'storyPoints' ? '故事点' : '工作项个数'} />];
    if (chartType === 'stackedBar') {
      columns.length = 0;
      columns.splice(0, 0, ...(data || []).map((i, index) => (
        <Column
          name={`value-${i.comparedId}`}
          header={<Tooltip title={i.comparedValue}>{i.comparedValue}</Tooltip>}
          hidden={index > 100}
        />
      )));
    }
    if (chartType === 'pie') {
      columns.push(<Column name="percentage" header="占比" />);
    }
    if (chartType === 'stackedBar') {
      columns.push(<Column name="total" header="总计" lock={'right' as any} />);
    }
    return columns;
  }, [chartType, data, type]);
  return (
    <Table
      dataSet={dataSet}
      style={{ padding: '0 .2rem' }}
      pagination={{
        onChange: (newPage, newPageSize) => {
          loadData(newPage, newPageSize);
          // dataSet.deleteAll(false);
        },
      }}
    >
      <Column name="analysisValue" width={180} lock={'left' as any} tooltip={'overflow' as any} />
      {renderColumns()}
    </Table>
  );
};
export default CustomReportTable;
