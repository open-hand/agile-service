/* eslint-disable react/forbid-prop-types */
import React from 'react';
import { AutoSizer, Grid, ScrollSync } from 'react-virtualized';
import PropTypes from 'prop-types';
import scrollbarSize from 'dom-helpers/scrollbarSize';
import styles from './Table.less';

Table.propTypes = {
  data: PropTypes.array.isRequired,
  columns: PropTypes.array.isRequired,
};
export default class Table extends React.PureComponent {
  constructor(props, context) {
    super(props, context);
    this.state = {
      columnWidth: 75,
      overscanColumnCount: 0,
      overscanRowCount: 10,
      rowHeight: 40,
    };

    this.renderBodyCell = this.renderBodyCell.bind(this);
    this.renderHeaderCell = this.renderHeaderCell.bind(this);
    this.renderLeftHeaderCell = this.renderLeftHeaderCell.bind(this);
    this.renderLeftSideCell = this.renderLeftSideCell.bind(this);
  }

  renderBodyCell({
    columnIndex, key, rowIndex, style,
  }) {
    if (columnIndex < 1) {
      return null;
    }

    return this.renderLeftSideCell({
      columnIndex, key, rowIndex, style,
    });
  }

  renderHeaderCell({
    columnIndex, key, rowIndex, style,
  }) {
    if (columnIndex < 1) {
      return null;
    }

    return this.renderLeftHeaderCell({
      columnIndex, key, rowIndex, style,
    });
  }

  renderLeftHeaderCell({ columnIndex, key, style }) {
    const { columns } = this.props;
    const column = columns[columnIndex];
    return (
      <div className={styles.headerCell} key={key} style={style}>
        {`${column.name}`}
      </div>
    );
  }

  renderLeftSideCell({
    columnIndex, key, rowIndex, style,
  }) {
    const { data, columns } = this.props;
    const column = columns[columnIndex];
    const dataIndex = column.name;
    return (
      <div className={styles.cell} key={key} style={style}>
        {column.renderer ? column.renderer() : data[rowIndex][dataIndex]}
      </div>
    );
  }

  render() {
    const {
      columnWidth,
      overscanColumnCount,
      overscanRowCount,
      rowHeight,
    } = this.state;
    const { data, columns } = this.props;
    const columnCount = columns.length;
    const rowCount = data.length;
    return (
      <AutoSizer>
        {({ height: originHeight, width }) => {
          const height = originHeight - 80;
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
              }) => {
                const leftColor = '#000';
                const topColor = '#000';
                const middleColor = '#000';
                const headerBackgroundColor = '#F5F5F5';
                return (
                  <div style={{ width, height }} className={styles.GridRow}>
                    <div
                      className={styles.LeftSideGridContainer}
                      style={{
                        position: 'absolute',
                        left: 0,
                        top: 0,
                        color: leftColor,
                        backgroundColor: headerBackgroundColor,
                      }}
                    >
                      <Grid
                        cellRenderer={this.renderLeftHeaderCell}
                        className={styles.HeaderGrid}
                        width={columnWidth}
                        height={rowHeight}
                        rowHeight={rowHeight}
                        columnWidth={columnWidth}
                        rowCount={1}
                        columnCount={1}
                      />
                    </div>
                    <div
                      className={styles.LeftSideGridContainer}
                      style={{
                        position: 'absolute',
                        left: 0,
                        top: rowHeight,
                        color: leftColor,
                        backgroundColor: 'white',
                      }}
                    >
                      <Grid
                        overscanColumnCount={overscanColumnCount}
                        overscanRowCount={overscanRowCount}
                        cellRenderer={this.renderLeftSideCell}
                        columnWidth={columnWidth}
                        columnCount={1}
                        className={styles.LeftSideGrid}
                        height={height - scrollbarSize()}
                        rowHeight={rowHeight}
                        rowCount={rowCount}
                        scrollTop={scrollTop}
                        width={columnWidth}
                      />
                    </div>
                    <div className={styles.GridColumn}>
                      {/* <AutoSizer disableHeight>
                      {({ width }) => ( */}
                      <div>
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
                            columnWidth={columnWidth}
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
                            columnWidth={columnWidth}
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
                      {/* )}
                    </AutoSizer> */}
                    </div>
                    <div
                      className={styles.LeftSideGridContainer}
                      style={{
                        position: 'absolute',
                        right: scrollbarSize(),
                        top: 0,
                        color: leftColor,
                        backgroundColor: headerBackgroundColor,
                      }}
                    >
                      <Grid
                        cellRenderer={this.renderLeftHeaderCell}
                        className={styles.HeaderGrid}
                        width={columnWidth}
                        height={rowHeight}
                        rowHeight={rowHeight}
                        columnWidth={columnWidth}
                        rowCount={1}
                        columnCount={1}
                      />
                    </div>
                    <div
                      className={styles.LeftSideGridContainer}
                      style={{
                        position: 'absolute',
                        right: scrollbarSize(),
                        top: rowHeight,
                        color: leftColor,
                        backgroundColor: 'white',
                      }}
                    >
                      <Grid
                        overscanColumnCount={overscanColumnCount}
                        overscanRowCount={overscanRowCount}
                        cellRenderer={this.renderLeftSideCell}
                        columnWidth={columnWidth}
                        columnCount={1}
                        className={styles.LeftSideGrid}
                        height={height - scrollbarSize()}
                        rowHeight={rowHeight}
                        rowCount={rowCount}
                        scrollTop={scrollTop}
                        width={columnWidth}
                      />
                    </div>
                  </div>
                );
              }}
            </ScrollSync>
          );
        }}
      </AutoSizer>
    );
  }
}
