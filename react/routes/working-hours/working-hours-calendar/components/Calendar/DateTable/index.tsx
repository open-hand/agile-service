/* eslint-disable react-hooks/exhaustive-deps */
import React, {
  useCallback, useMemo, useState, useRef, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import originMoment, { Moment } from 'moment';
import { Icon, Tooltip } from 'choerodon-ui';
import { extendMoment } from 'moment-range';
import classNames from 'classnames';
import { useSize } from 'ahooks';
import {
  cloneDeep, debounce, max, sum,
} from 'lodash';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { ICalendarData, useCalendarStore } from '../../../stores';
import styles from './index.less';
import UserTag from '@/components/tag/user-tag';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { workingHoursApi } from '@/api';
import { ConstantNum } from './utils';
import { getIsOrganization } from '@/utils/common';

const {
  cardHeight, cardMargin, cardPadding1, cardPadding2, issuePadding1, issuePadding2, countHeight,
} = ConstantNum;
// @ts-ignore
const moment = extendMoment(originMoment);

const dayMap = new Map([
  ['0', '周日'],
  ['1', '周一'],
  ['2', '周二'],
  ['3', '周三'],
  ['4', '周四'],
  ['5', '周五'],
  ['6', '周六'],
]);

interface ICalendarIssue {
  summary: string
  workTime: number
  estimateTime: number
  issueId: string
  projectId: string
}

interface Props {
  dateTableWrapperSize: {
    width?: number,
    height?: number,
  }
}

const DateTable: React.FC<Props> = ({ dateTableWrapperSize }) => {
  const {
    searchDs, isRestDay, calendarDs, countData, setLoading, getCountData, AppState,
  } = useCalendarStore();
  const startDate = useMemo(() => searchDs.current?.get('startTime')?.toString(), [searchDs.current?.get('startTime')]);
  const endDate = useMemo(() => searchDs.current?.get('endTime')?.toString(), [searchDs.current?.get('endTime')]);
  const startTime = moment(startDate).startOf('day').format('YYYY-MM-DD HH:mm:ss');
  const endTime = moment(endDate).endOf('day').format('YYYY-MM-DD HH:mm:ss');
  const dateTableRef = useRef();
  const dateTableSize = useSize(dateTableRef);
  const [widthPerDay, setWidthPerDay] = useState(0);
  const [expandMap, setExpandMap] = useState<Map<string, boolean>>(new Map([]));
  const [userIssuesMap, setUserIssuesMap] = useState<Map<string, {[date: string]: ICalendarIssue[]}>>(new Map());
  const [userIssuesHeightMap, setUserIssuesHeightMap] = useState<Map<string, number>>(new Map());
  const [defaultScrolled, setDefaultScrolled] = useState<boolean>(false);
  const [firstSetDefaultScrolled, setFirstSetDefaultScrolled] = useState<boolean>(false);
  const appEle = document.querySelector('#app');
  const menuEle = document.querySelector('#menu');
  // @ts-ignore
  const appEleSize = useSize(appEle);
  // @ts-ignore
  const menuEleSize = useSize(menuEle);
  const [issueDetailProps] = useDetail();

  useEffect(debounce(() => {
    if (appEleSize.width && menuEleSize.width) {
      if ((appEleSize.width - menuEleSize.width - 24 - 250) / 7 !== widthPerDay) {
        setWidthPerDay((appEleSize.width - menuEleSize.width - 24 - 250) / 7);
      }
    }
  }, 300), [appEleSize.width, menuEleSize.width]);

  useEffect(() => {
    if (!firstSetDefaultScrolled && dateTableSize.height && dateTableWrapperSize.height) {
      batchedUpdates(() => {
        // @ts-ignore
        setDefaultScrolled(dateTableSize.height > dateTableWrapperSize.height);
        setFirstSetDefaultScrolled(true);
      });
    }
  }, [firstSetDefaultScrolled, dateTableSize.height, dateTableWrapperSize.height]);

  const getBetweenDate = useCallback((start: Moment, end: Moment): {
    date: string,
    week: number,
    isToday:boolean,
    isRestDay: boolean,
    format: string,
    left: number,
    width: number,
  }[] => {
    if (start && end && widthPerDay) {
      // 是否显示非工作日
      const range = moment.range(start, end);
      const days = Array.from(range.by('day'));
      return days.map((day, i) => ({
        date: day.format('MM/DD'),
        week: day.day(),
        isToday: day.format('YYYY-MM-DD') === moment().format('YYYY-MM-DD'),
        isRestDay: isRestDay(day),
        format: day.format('YYYY-MM-DD'),
        left: i * widthPerDay,
        width: widthPerDay,
      }));
    }
    return [];
  }, [isRestDay, widthPerDay]);

  const betweenDate = useMemo(() => getBetweenDate(startDate, endDate), [endDate, getBetweenDate, startDate]);

  const renderDate = useCallback(() => betweenDate.map((item) => (
    <div
      className={classNames(styles.header_cell, styles.cell,
        {
          [styles.header_toDayCell]: item.isToday,
          [styles.header_restDayCell]: item.isRestDay,
        })}
      style={{ left: item.left, width: item.width }}
    >
      <span className={styles.header_cell_date}>{item.date}</span>
      <span className={styles.header_cell_week}>{dayMap.get(item.week.toString())}</span>
    </div>
  )), [betweenDate]);

  useEffect(() => { // 更改筛选条件时，全部折叠，避免没有请求展开的user的登记出现问题
    batchedUpdates(() => {
      setUserIssuesHeightMap(new Map());
      setUserIssuesMap(new Map());
      setExpandMap(new Map());
    });
  }, [
    searchDs.current?.get('startTime'),
    searchDs.current?.get('endTime'),
    searchDs.current?.get('userIds'),
    searchDs.current?.get('projectIds'),
  ]);
  const handleExpand = useCallback((item: ICalendarData) => {
    const newMap = cloneDeep(expandMap);
    const newUserIssuesMap = cloneDeep(userIssuesMap);
    const newUserIssuesHeightMap = cloneDeep(userIssuesHeightMap);
    if (!newMap.get(item.userId)) {
      newMap.set(item.userId, true);
      setLoading(true);
      workingHoursApi.getUserCalendar(item.userId, {
        startTime,
        endTime,
        projectIds: searchDs.current?.get('projectIds'),
      }).then((data: {[date: string]: ICalendarIssue[]}) => {
        newUserIssuesMap.set(item.userId, data);
        let maxCellIssuesHeight = 0;
        if (data) {
          const mostCount = max(Object.values(data).map((issues) => issues.length)) || 0;
          if (mostCount > 0) {
            maxCellIssuesHeight = mostCount * (cardMargin + cardHeight) - cardMargin + issuePadding1 + issuePadding2;
          }
        }
        newUserIssuesHeightMap.set(item.userId, maxCellIssuesHeight);
        batchedUpdates(() => {
          setLoading(false);
          setUserIssuesMap(newUserIssuesMap);
          setUserIssuesHeightMap(newUserIssuesHeightMap);
        });
      });
    } else {
      newMap.delete(item.userId);
      newUserIssuesHeightMap.delete(item.userId);
      newUserIssuesHeightMap.delete(item.userId);
    }
    batchedUpdates(() => {
      setExpandMap(newMap);
      setUserIssuesHeightMap(newUserIssuesHeightMap);
      setUserIssuesMap(newUserIssuesMap);
    });
  }, [expandMap, userIssuesMap, userIssuesHeightMap, startTime, endTime, searchDs.current?.get('projectIds')]);

  const detailCallback = useCallback(() => {
    setLoading(true);
    const userId = AppState.userInfo.id;
    const newUserIssuesMap = cloneDeep(userIssuesMap);
    const newUserIssuesHeightMap = cloneDeep(userIssuesHeightMap);
    workingHoursApi.getUserCalendar(userId, {
      startTime,
      endTime,
      projectIds: searchDs.current?.get('projectIds'),
    }).then((data: {[date: string]: ICalendarIssue[]}) => {
      if (expandMap.get(userId)) {
        newUserIssuesMap.set(userId, data);
        let maxCellIssuesHeight = 0;
        if (data) {
          const mostCount = max(Object.values(data).map((issues) => issues.length)) || 0;
          if (mostCount > 0) {
            maxCellIssuesHeight = mostCount * (cardMargin + cardHeight) - cardMargin + issuePadding1 + issuePadding2;
          }
        }
        newUserIssuesHeightMap.set(userId, maxCellIssuesHeight);
      }

      const userRecord = calendarDs.find((record) => record.get('userId') === userId);
      if (userRecord && data) {
        const newCountMap = {};
        Object.entries(data).forEach(([date, values]) => {
          // @ts-ignore
          newCountMap[date] = sum((values || []).map((value) => value.workTime || 0));
        });
        userRecord.set('countMap', newCountMap);
        userRecord.set('allEstimateTime', sum(Object.values(newCountMap)));
      }

      batchedUpdates(() => {
        setLoading(false);
        if (expandMap.get(userId)) {
          setUserIssuesMap(newUserIssuesMap);
          setUserIssuesHeightMap(newUserIssuesHeightMap);
        }
      });
    });
    getCountData({
      startTime,
      endTime,
      userIds: searchDs.current?.get('userIds'),
      projectIds: searchDs.current?.get('projectIds'),
    });
  }, [
    searchDs.current?.get('userIds'),
    searchDs.current?.get('projectIds'),
    startTime,
    endTime,
    expandMap,
    userIssuesMap,
    userIssuesHeightMap,
  ]);

  const openIssueDetail = useCallback((e, issue) => {
    e.stopPropagation();
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: issue.issueId,
        projectId: issue.projectId,
        applyType: 'agile',
        disabled: getIsOrganization(),
      },
      events: {
        delete: detailCallback,
        close: detailCallback,
      },
    });
  }, [issueDetailProps, detailCallback]);

  const renderRows = useCallback(() => (
    <>
      {
        calendarDs.toData().map((item: ICalendarData) => {
          const maxCellIssuesHeight = expandMap.get(item.userId) ? (userIssuesHeightMap.get(item.userId) || 0) : 0;
          return (
            <div
              className={classNames(styles.row, {
                [styles.expandRow]: expandMap.get(item.userId),
                [styles.lastCellHasBorderRow]: betweenDate.length < 7,
              })}
              style={{
                width: 250 + betweenDate.length * widthPerDay,
                height: countHeight + maxCellIssuesHeight,
                minHeight: countHeight,
              }}
            >
              <div className={classNames(styles.firstCell, styles.row_firstCell, styles.row_cell, styles.body_cell)}>
                <div style={{ marginLeft: (item.allEstimateTime || 0) > 0 ? 0 : 20 }}>
                  {
                  (item.allEstimateTime || 0) > 0 && (
                    <Icon
                      className={styles.row_firstCell_icon}
                      type={expandMap.get(item.userId) ? 'expand_more' : 'navigate_next'}
                      onClick={() => handleExpand(item)}
                    />
                  )
                }
                  <UserTag data={item.userMessageDTO} />
                </div>
                <div>{`${item.allEstimateTime || 0}h`}</div>
              </div>
              <div
                className={styles.otherCells}
                style={{ width: 7 * widthPerDay }}
              >
                {
              betweenDate.map((date) => {
                const dateCount = item.countMap?.[date.format] || 0;
                let cellColor = '#edfffd';
                if (dateCount > 8) {
                  cellColor = '#FFD9CD';
                }
                if (dateCount < 8) {
                  cellColor = '#FFF6E1';
                }
                if (date.isRestDay) {
                  cellColor = '#FFF';
                }
                return (
                  <div
                    className={classNames(styles.row_cell, styles.cell, styles.body_cell)}
                    style={{
                      left: date.left,
                      width: date.width,
                      height: countHeight + maxCellIssuesHeight,
                      minHeight: countHeight,
                    }}
                  >
                    <div
                      className={styles.body_cell_count}
                      style={{
                        background: cellColor,
                        height: countHeight - 1,
                        lineHeight: `${countHeight - 1}px`,
                        color: date.isRestDay ? 'rgba(15,19,88,0.45)' : 'var(--text-color)',
                      }}
                    >
                      { `${dateCount}h`}
                    </div>
                    {
                      expandMap.get(item.userId) && userIssuesMap.get(item.userId) && (
                        <div
                          className={styles.body_cell_issues}
                          style={{
                            padding: `${issuePadding1}px ${issuePadding1}px ${issuePadding2}px ${issuePadding1}px`,
                          }}
                        >
                          {
                            // @ts-ignore
                            userIssuesMap.get(item.userId)?.[date.format]?.map((issue: ICalendarIssue, i, arr) => (
                              <div
                                role="none"
                                className={styles.issueCard}
                                style={{
                                  height: cardHeight,
                                  padding: `${cardPadding1}px ${cardPadding2}px`,
                                  marginBottom: i + 1 === arr.length ? 0 : cardMargin,
                                }}
                                onClick={(e) => openIssueDetail(e, issue)}
                              >
                                <Tooltip title={issue.summary}>
                                  <div className={styles.issueCard_summary}>{issue.summary}</div>
                                </Tooltip>
                                <div>
                                  <div className={styles.issue_time}>
                                    <span className={styles.issue_time_label}>实际工时</span>
                                    <span>{`${issue.workTime || 0}h`}</span>
                                  </div>
                                  <div className={styles.issue_time}>
                                    <span className={styles.issue_time_label}>预估工时</span>
                                    <span>{`${issue.estimateTime || 0}h`}</span>
                                  </div>
                                </div>
                              </div>
                            ))
                          }
                        </div>
                      )
                    }
                  </div>
                );
              })
            }
              </div>
            </div>
          );
        })
      }
    </>
  ), [betweenDate, calendarDs, expandMap, handleExpand, userIssuesMap, userIssuesHeightMap]);

  const renderEmptyBlock = useCallback(() => {
    const blockHeight = (dateTableWrapperSize.height || 0) - ((calendarDs.toData().length) * countHeight + sum([...userIssuesHeightMap.values()]) + (calendarDs.totalPage > calendarDs.currentPage ? 40 : 0)) - 100; // 96
    return blockHeight > 0 && (
    <div
      className={classNames(styles.row, styles.blockRow, {
        [styles.lastCellHasBorderRow]: betweenDate.length < 7,
      })}
      style={{
        width: 250 + betweenDate.length * widthPerDay,
        height: blockHeight,
      }}
    >
      <div
        className={classNames(styles.firstCell, styles.row_firstCell, styles.row_cell)}
        style={{
          height: blockHeight,
        }}
      />
      <div
        className={styles.otherCells}
        style={{
          width: 7 * widthPerDay,
          height: blockHeight,
        }}
      >
        {
          betweenDate.map((date) => (
            <div
              className={classNames(styles.row_cell, styles.cell)}
              style={{
                left: date.left,
                width: date.width,
                height: blockHeight,
              }}
            />
          ))
        }
      </div>
    </div>
    );
  },
  [betweenDate, dateTableWrapperSize.height, userIssuesHeightMap, calendarDs]);

  const renderFooter = useCallback(() => (
    <div
      className={classNames(styles.row, styles.footer, {
        [styles.lastCellHasBorderRow]: betweenDate.length < 7,
      })}
      style={{ width: 250 + betweenDate.length * widthPerDay }}
    >
      <div className={classNames(styles.firstCell, styles.footer_firstCell, styles.row_cell, styles.footer_cell)}>
        <div>{`总计实际工时：${sum(Object.values(countData))}小时`}</div>
      </div>
      <div
        className={styles.otherCells}
        style={{ width: 7 * widthPerDay }}
      >
        {
          betweenDate.map((date) => (
            <div
              className={classNames(styles.footer_cell, styles.cell, styles.row_cell)}
              style={{ left: date.left, width: date.width }}
            >
              {`${countData[date.format] || 0}小时`}
            </div>
          ))
        }
      </div>
    </div>
  ), [betweenDate, countData]);

  const handleLoadMore = useCallback(() => {
    calendarDs.queryMore(calendarDs.currentPage + 1);
  }, []);

  return (
    <div
      className={styles.dateTable}
      style={{
        // width: '100%',
        width: 250 + 7 * widthPerDay,
        maxWidth: widthPerDay ? 250 + 7 * widthPerDay - (((dateTableWrapperSize.height || 0) < (dateTableSize.height || 0) && !defaultScrolled) ? 4 : 0) : '100%', // -2,滚动条
      }}
    // @ts-ignore
      ref={dateTableRef}
    >
      {
        startDate && endDate && calendarDs.toData().length > 0 && (
          <>
            <div
              className={classNames(styles.header, { [styles.lastCellHasBorderRow]: betweenDate.length < 7 })}
              style={{ width: 250 + betweenDate.length * widthPerDay }}
            >
              <div className={classNames(styles.firstCell, styles.header_firstCell, styles.header_cell)}>
                <span>成员信息</span>
                <span>总计实际工时</span>
              </div>
              <div
                className={styles.otherCells}
                style={{ width: 7 * widthPerDay }}
              >
                {renderDate()}
              </div>
            </div>
            <div
              className={styles.body}
              style={{
                width: 250 + betweenDate.length * widthPerDay,
                height: (calendarDs.toData().length) * countHeight + sum([...userIssuesHeightMap.values()]) + (calendarDs.totalPage > calendarDs.currentPage ? 40 : 0),
              }}
            >
              {renderRows()}
              {
                calendarDs.totalPage > calendarDs.currentPage && (
                <div
                  className={styles.more}
                  style={{ width: 250 + betweenDate.length * widthPerDay }}
                  role="none"
                  onClick={handleLoadMore}
                >
                  <span
                    className={styles.more_span}
                    style={{
                      left: ((250 + 7 * widthPerDay) - 75) / 2,
                      right: ((250 + 7 * widthPerDay) - 75) / 2,
                    }}
                  >
                    <span>查看更多</span>
                    <Icon
                      className={styles.more_span_icon}
                      type="expand_more"
                    />
                  </span>
                </div>
                )
              }
            </div>
            {
              renderEmptyBlock()
            }
            {renderFooter()}
          </>
        )
      }
      <DetailContainer {...issueDetailProps} />
    </div>
  );
};

export default observer(DateTable);
