import React, { useCallback, useEffect, useMemo } from 'react';
import { map } from 'lodash';
import { Icon, TextField } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useWorkCalendarStore } from '@/routes/work-calendar/stores';
import { IssueItem } from '@/routes/work-calendar/types';
import Style from './index.less';

interface Props {
  refresh(): void,
  openEditIssue(data: { event: { id: string, extendedProps: { projectId: string } } }): void,
}

const IssueList = ({ refresh, openEditIssue }: Props) => {
  const {
    mainStore,
  } = useWorkCalendarStore();

  const detailOption = useMemo(() => [{
    name: '预计开始时间',
    value: 'estimatedStartTime',
  }, {
    name: '预计结束时间',
    value: 'estimatedEndTime',
  }, {
    name: '已完成工作项数',
    value: 'completedCount',
    hasBackground: true,
  }, {
    name: '总工作项数',
    value: 'totalCount',
    hasBackground: true,
  }], []);

  useEffect(() => {
    loadList();
  }, []);

  const loadList = useCallback(() => {
    mainStore.loadIssueList();
  }, [mainStore]);

  const handleClick = useCallback((value: string) => {
    if (mainStore.getFilterIssueId === value) {
      mainStore.setFilterIssueId(null);
    } else {
      mainStore.setFilterIssueId(value);
    }
    refresh();
  }, [mainStore]);

  const handleExpand = useCallback((e, issueId: string) => {
    e.stopPropagation();

    const { expandMap } = mainStore;
    expandMap.set(issueId, !expandMap.get(issueId) || false);
  }, [mainStore]);

  const openIssueDetail = useCallback((e, item: IssueItem) => {
    e.stopPropagation();
    openEditIssue({
      event: {
        id: item.issueId,
        extendedProps: { projectId: item?.projectId },
      },
    });
  }, [openEditIssue]);

  const isExpand = useCallback((issueId: string) => {
    const { expandMap } = mainStore;
    return expandMap.get(issueId);
  }, [mainStore]);

  const handleSearch = useCallback((value) => {
    mainStore.setSearchParams(value);
    loadList();
  }, [mainStore, loadList]);

  return (
    <>
      <TextField
        placeholder="请输入搜索内容"
        prefix={<Icon type="search" />}
        className={Style.search}
        clearButton
        onChange={handleSearch}
      />
      {map(mainStore.getIssueList, (item: IssueItem) => (
        <div
          key={item.issueId}
          role="none"
          onClick={() => handleClick(item.issueId)}
          className={`${Style.wrap} ${mainStore.getFilterIssueId === item.issueId ? Style.wrapSelected : ''}`}
        >
          <div className={Style.header}>
            <Icon
              type={isExpand(item.issueId) ? 'baseline-arrow_drop_down' : 'baseline-arrow_right'}
              onClick={(e) => handleExpand(e, item.issueId)}
            />
            <span className={Style.summary}>{item.summary}</span>
          </div>
          <div className={Style.progressWrap}>
            <div
              className={Style.progressDone}
              style={{ flex: item.completedCount }}
            />
            <div
              className={Style.progressTodo}
              style={{ flex: item.totalCount ? item.totalCount - item.completedCount : 1 }}
            />
          </div>
          {isExpand(item.issueId) && (
            <div className={Style.issueDetail}>
              <div className={Style.issueDetailLink}>
                <span className={Style.label}>任务详情</span>
                <span
                  role="none"
                  className={Style.link}
                  onClick={(e) => openIssueDetail(e, item)}
                >
                  查看详情
                </span>
              </div>
              <div>
                {map(detailOption, ({ name, value, hasBackground = false }) => (
                  <div className={Style.detailOption}>
                    <span className={Style.label}>{name}</span>
                    {/* @ts-ignore */}
                    <span className={hasBackground ? Style.value : ''}>{item[value]}</span>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>
      ))}
    </>
  );
};

export default observer(IssueList);