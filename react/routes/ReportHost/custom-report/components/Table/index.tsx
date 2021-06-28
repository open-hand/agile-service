import React, { useCallback, useMemo } from 'react';
import {
  Button, DataSet, TextField, Form, Table,
} from 'choerodon-ui/pro';
import getOptions, { IChartData, IChartType, IChartUnit } from '../Chart/utils';

interface CustomReportTableProps{
    loading: boolean,
    data: IChartData[] | null,
    chartType?: IChartType,
    type?: IChartUnit,
}
const { Column } = Table;
const CustomReportTable:React.FC<CustomReportTableProps> = ({ data }) => {
  const dataSet = useMemo(() => new DataSet({}), []);
  const renderColumns = useCallback(() => {
    const name = [];
    return <Column />;
  }, []);
  return (
    <Table dataSet={dataSet}>
      {renderColumns()}
    </Table>
  );
};
export default CustomReportTable;
