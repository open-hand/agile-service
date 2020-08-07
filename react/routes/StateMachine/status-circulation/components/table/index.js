/* eslint-disable react/sort-comp */
/* eslint-disable react/forbid-prop-types */
import React from 'react';
import { AutoSizer, Grid, ScrollSync } from 'react-virtualized';
import PropTypes from 'prop-types';
import scrollbarSize from 'dom-helpers/scrollbarSize';
import styles from './index.less';

const leftColor = '#000';
const topColor = '#000';
const middleColor = '#000';
const headerBackgroundColor = '#F5F5F5';

export default class Table extends React.PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      columnWidth: 100,
      overscanColumnCount: 0,
      overscanRowCount: 10,
      rowHeight: 40,
    };
  }

  renderBodyCell=({
    columnIndex, key, rowIndex, style,
  }) => {
    const { columns } = this.props;
    const { left, right } = this.getFixedColumns();
    if (columnIndex < left.length || columnIndex > columns.length - right.length - 1) {
      return null;
    }

    return this.renderLeftSideCell({
      columnIndex, key, rowIndex, style,
    });
  }

  renderHeaderCell=({
    columnIndex, key, rowIndex, style,
  }) => {
    const { columns } = this.props;
    const { left, right } = this.getFixedColumns();
    if (columnIndex < left.length || columnIndex > columns.length - right.length - 1) {
      return null;
    }

    return this.renderLeftHeaderCell({
      columnIndex, key, rowIndex, style,
    });
  }

  renderLeftHeaderCell=({ columnIndex, key, style }) => {
    const { columns } = this.props;
    const column = columns[columnIndex];
    return (
      <div className={styles.headerCell} key={key} style={style}>
        {column.renderHeader ? column.renderHeader(column) : column.name}
      </div>
    );
  }

  renderLeftSideCell=({
    columnIndex, key, rowIndex, style,
  }) => {
    const { data, columns } = this.props;
    const column = columns[columnIndex];
    const dataIndex = column.name;
    return (
      <div className={styles.cell} key={key} style={style}>
        {column.renderer ? column.renderer(data[rowIndex], rowIndex) : data[rowIndex][dataIndex]}
      </div>
    );
  }

  getFixedColumns = () => {
    const { columns } = this.props;
    const fixedLeftColumns = columns.filter((column) => column.lock === true);
    const fixedRightColumns = columns.filter((column) => column.lock === 'right');
    return {
      left: fixedLeftColumns,
      right: fixedRightColumns,
    };
  }

  getFixedColumnIndex = (fixedColumns, mode, originColumnIndex) => {
    const { columns } = this.props;
    if (mode === 'left') {
      return originColumnIndex;
    } if (mode === 'right') {
      return columns.length - fixedColumns.length + originColumnIndex;
    }
    return 0;
  }

  renderFixedLeftColumn = (fixedLeftColumns, { ...props }) => {
    if (fixedLeftColumns.length > 0) {
      return this.renderFixedColumn({
        ...props,
        mode: 'left',
        fixedColumns: fixedLeftColumns,
      });
    }
    return null;
  }

  renderFixedRightColumn = (fixedRightColumns, { ...props }) => {
    if (fixedRightColumns.length > 0) {
      return this.renderFixedColumn({
        ...props,
        mode: 'right',
        fixedColumns: fixedRightColumns,
      });
    }
    return null;
  }

  renderFixedColumn = ({
    height, mode, fixedColumns, rowCount, scrollTop, left, right,
  }) => {
    const {
      columnWidth,
      overscanColumnCount,
      overscanRowCount,
      rowHeight,
    } = this.state;
    const columnCount = fixedColumns.length;
    const width = columnWidth * columnCount;
    return (
      <>
        <div
          className={styles.LeftSideGridContainer}
          style={{
            position: 'absolute',
            left: mode === 'left' ? 0 : undefined,
            right: mode === 'left' ? undefined : scrollbarSize(),
            top: 0,
            color: leftColor,
            backgroundColor: headerBackgroundColor,
          }}
        >
          <Grid
            cellRenderer={({ columnIndex, ...props }) => this.renderLeftHeaderCell({
              ...props,
              columnIndex: this.getFixedColumnIndex(fixedColumns, mode, columnIndex),
            })}
            className={styles.HeaderGrid}
            width={width}
            height={rowHeight}
            rowHeight={rowHeight}
            columnWidth={columnWidth}
            rowCount={1}
            columnCount={columnCount}
          />
        </div>
        <div
          className={styles.LeftSideGridContainer}
          style={{
            position: 'absolute',
            left: mode === 'left' ? 0 : undefined,
            right: mode === 'left' ? undefined : scrollbarSize(),
            top: rowHeight,
            color: leftColor,
            backgroundColor: 'white',
          }}
        >
          <Grid
            overscanColumnCount={overscanColumnCount}
            overscanRowCount={overscanRowCount}
            cellRenderer={({ columnIndex, ...props }) => this.renderLeftSideCell({
              ...props,
              columnIndex: this.getFixedColumnIndex(fixedColumns, mode, columnIndex),
            })}
            columnWidth={columnWidth}
            columnCount={columnCount}
            className={styles.LeftSideGrid}
            height={height - scrollbarSize()}
            rowHeight={rowHeight}
            rowCount={rowCount}
            scrollTop={scrollTop}
            width={width}
          />
        </div>
      </>
    );
  }

  getColumnWidth=({ index, width }) => {
    const {
      columnWidth,
    } = this.state;
    const { columns } = this.props;
    const { left, right } = this.getFixedColumns();
    if (index < left.length || index > columns.length - right.length - 1) {
      return columnWidth;
    }
    const MIN_WIDTH = 100;
    const fixedWidth = (left.length + right.length) * columnWidth;
    const remainWidth = width - fixedWidth;
    const columnCount = columns.length - (left.length + right.length);
    return Math.max(MIN_WIDTH, remainWidth / columnCount);
  }

  renderColumns = ({
    width, height, scrollLeft, onScroll, rowCount,
  }) => {
    const {
      overscanColumnCount,
      overscanRowCount,
      rowHeight,
    } = this.state;
    const { columns } = this.props;
    const columnCount = columns.length;
    return (
      <div className={styles.GridColumn}>
        <div
          style={{
            color: topColor,
            backgroundColor: headerBackgroundColor,
            height: rowHeight,
            width: width - scrollbarSize(),
          }}
        >
          <Grid
            className={styles.HeaderGrid}
            columnWidth={({ index }) => this.getColumnWidth({ index, width })}
            columnCount={columnCount}
            height={rowHeight}
            overscanColumnCount={overscanColumnCount}
            cellRenderer={this.renderHeaderCell}
            rowHeight={rowHeight}
            rowCount={1}
            scrollLeft={scrollLeft}
            width={width - scrollbarSize()}
          />
        </div>
        <div
          style={{
            color: middleColor,
            backgroundColor: 'white',
            height,
            width,
          }}
        >
          <Grid
            className={styles.BodyGrid}
            columnWidth={({ index }) => this.getColumnWidth({ index, width })}
            columnCount={columnCount}
            height={height}
            onScroll={onScroll}
            overscanColumnCount={overscanColumnCount}
            overscanRowCount={overscanRowCount}
            cellRenderer={this.renderBodyCell}
            rowHeight={rowHeight}
            rowCount={rowCount}
            width={width}
          />
        </div>
      </div>
    );
  }

  render() {
    const { data } = this.props;
    const rowCount = data.length;
    const { left, right } = this.getFixedColumns();
    return (
      <AutoSizer>
        {({ height: originHeight, width }) => {
          const height = originHeight - 40;
          return (
            <ScrollSync>
              {({
                clientHeight,
                clientWidth,
                onScroll,
                scrollHeight,
                scrollLeft,
                scrollTop,
                scrollWidth,
              }) => (
                <div style={{ width, height }} className={styles.GridRow}>
                  {this.renderFixedLeftColumn(left, { height, rowCount, scrollTop })}
                  {this.renderColumns({
                    width, height, scrollLeft, onScroll, rowCount,
                  })}
                  {this.renderFixedRightColumn(right, { height, rowCount, scrollTop })}
                </div>
              )}
            </ScrollSync>
          );
        }}
      </AutoSizer>
    );
  }
}
Table.propTypes = {
  data: PropTypes.array.isRequired,
  columns: PropTypes.array.isRequired,
};
