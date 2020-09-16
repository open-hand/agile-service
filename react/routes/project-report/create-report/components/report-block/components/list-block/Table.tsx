import React from 'react';
import styles from './Table.less';

interface Column<T = {}> {
  title: string
  dataIndex: string
  render?: (record: T) => React.ReactNode
}
interface Props<T> {
  columns: Column<T>[]
  data: T[]
}
export function createColumns<T extends {}>(columns: Column<T>[]): Column<T>[] {
  return columns;
}
function Table<T extends { [key: string]: any }>({
  columns,
  data,
}: Props<T>) {
  return (
    <table className={styles.table}>
      <thead>
        <tr>
          {columns.map((column) => <th>{column.title}</th>)}
        </tr>
      </thead>
      <tbody>
        {data.map((item) => (
          <tr>
            {columns.map((column) => <td>{column.render ? column.render(item) : item[column.dataIndex]}</td>)}
          </tr>
        ))}
      </tbody>
    </table>
  );
}
export default Table;
