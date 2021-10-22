import React, {
  useContext, useEffect, useState, useCallback, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { useHistory } from 'react-router-dom';
import {
  Header, Content, Page, Breadcrumb, Choerodon, useTheme,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import {
  map, set, get, pick,
} from 'lodash';
import { useUnmount, usePersistFn } from 'ahooks';
import openCreateIssue from '@/components/create-issue';
import Loading, { LoadingProvider } from '@/components/Loading';
import { projectApi } from '@/api/Project';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { issueApi } from '@/api';
import IssueSearch from '@/components/issue-search';
import openSaveFilterModal from '@/components/SaveFilterModal';
import { linkUrl } from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import useQueryString from '@/hooks/useQueryString';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import FilterManage from '@/components/FilterManage';
import DetailContainer, { useDetail } from '@/components/detail-container';
import TableModeSwitch from '@/components/tree-list-switch';
import handleOpenImport from '@/components/ImportIssue/ImportIssue';
import { TableCache } from '@/components/issue-table/Component';
import useTable from '@/hooks/useTable';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import openBatchDeleteModal from '@/components/BatchDeleteConfirm';
import BatchModal from './components/BatchModal';
import IssueTable from './components/issue-table';
import { openExportIssueModal } from './components/ExportIssue';
import IssueStore from '../../stores/project/issue/IssueStore';
import Store, { StoreProvider } from './stores';
import CollapseAll from './components/CollapseAll';
import Modal from './components/Modal';
import './index.less';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';

const defaultVisibleColumns = [
  'summary',
  'issueNum',
  'priority',
  'assignee',
  'status',
  'sprint',
  'reporter',
  'lastUpdateDate',
];
const defaultListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));
const Issue = observer(({ cached, updateCache }) => {
  const {
    projectId, issueSearchStore, changeTableListMode, tableListMode, hasBatchDeletePermission,
  } = useContext(Store);
  const [theme] = useTheme();
  const history = useHistory();
  const params = useQueryString();
  const [urlFilter, setUrlFilter] = useState(null);
  const [props] = useDetail();
  const { open } = props;
  const { data: tableFields } = useIssueTableFields();

  const handleCheckBefore = useCallback(() => {
    if (!issueSearchStore.batchAction && !hasBatchDeletePermission) {
      issueSearchStore.setBatchAction('edit');
    }
  }, [hasBatchDeletePermission, issueSearchStore]);

  const getTableData = useCallback(({
    page, sort, size, isTree,
  }) => {
    const search = issueSearchStore.getCustomFieldFilters();
    set(search, 'searchArgs.tree', isTree);
    return issueApi.loadIssues(page, size, sort, search);
  }, [issueSearchStore]);
  const tableProps = useTable(getTableData, {
    rowKey: 'issueId',
    isTree: !tableListMode,
    defaultPage: cached?.pagination?.current,
    defaultPageSize: cached?.pagination?.pageSize,
    // defaultVisibleColumns: cached?.visibleColumns ?? defaultVisibleColumns,
    autoQuery: false,
    checkBefore: handleCheckBefore,
  });
  useUnmount(() => updateCache({
    pagination: tableProps.pagination,
    visibleColumns: tableProps.visibleColumns,
  }));
  const { query } = tableProps;
  /**
   * 默认此次操作不是删除操作
   * 防止删除此页一条数据时页时停留当前页时出现无数据清空
   * @param {Boolean} isDelete  用于标记是否为删除操作
   */
  const refresh = useCallback((isDelete = false) => query(
    isDelete
      && tableProps.data.length === 1
      && tableProps.pagination.total > 1
      ? tableProps.pagination.current - 1
      : tableProps.pagination.current,
  ), [query, tableProps]);
  const filterParams = useMemo(() => {
    const filterParamKeys = ['paramChoose', 'paramCurrentVersion', 'paramCurrentSprint', 'paramId',
      'paramType', 'paramIssueId', 'paramName', 'paramOpenIssueId', 'detailTab', 'statusId', 'sprint', 'issueTypeId', 'assigneeId',
      'storyPointsNull', 'remainingTimeNull',
    ];
    return pick(params, filterParamKeys);
  }, [params]);
  const hasUrlFilter = useCallback((obj) => {
    const whiteList = ['type', 'category', 'id', 'name', 'organizationId'];
    return Object.keys(obj).some((key) => !whiteList.includes(key));
  }, []);

  const initFilter = usePersistFn(async () => {
    const {
      paramChoose, paramCurrentVersion, paramCurrentSprint, paramId,
      paramType, paramIssueId, paramName, paramOpenIssueId, detailTab, storyPointsNull, remainingTimeNull,
      paramCustomFieldId, paramCustomFieldName, ...searchArgs
    } = params;
    if (hasUrlFilter(filterParams)) {
      issueSearchStore.clearAllFilter();
      localPageCacheStore.clear();
    }
    Object.keys(searchArgs).forEach((key) => {
      const value = searchArgs[key];
      switch (key) {
        case 'statusId':
        case 'sprint':
        case 'issueTypeId':
        case 'assigneeId': {
          issueSearchStore.handleFilterChange(key, value.split(','));
          break;
        }
        default: break;
      }
    });
    let prefix = '';
    if (paramChoose) {
      if (paramChoose === 'version' && paramCurrentVersion) {
        issueSearchStore.handleFilterChange(paramChoose, [paramCurrentVersion]);
        prefix = '版本';
      }
      if (paramChoose === 'sprint' && paramCurrentSprint) {
        issueSearchStore.handleFilterChange(paramChoose, [paramCurrentSprint]);
        prefix = '冲刺';
      }
    }
    const prefixs = {
      assigneeId: '经办人',
      issueTypeId: '类型',
      priorityId: '优先级',
      statusId: '状态',
      version: '版本',
      component: '模块',
      sprint: '冲刺',
      epic: '史诗',
      label: '标签',
      reporterIds: '报告人',
      mainResponsibleIds: '主要负责人',
      participantIds: '主要参与人',
      environment: '环境',
    };
    if (paramType) {
      prefix = prefixs[paramType] || paramCustomFieldName;
      if (paramType !== 'customField') {
        issueSearchStore.handleFilterChange(paramType, [paramId]);
      } else {
        issueSearchStore.updateFilter({
          [paramCustomFieldId]: {
            isCustom: true,
            value: [paramId],
          },
        });
      }
    }
    if (storyPointsNull === 'true') {
      issueSearchStore.handleFilterChange('storyPointsNull', true);
      prefix = '';
    }
    if (remainingTimeNull === 'true') {
      issueSearchStore.handleFilterChange('remainingTimeNull', true);
      prefix = '';
    }
    setUrlFilter(`${prefix ? `${prefix}:` : ''}${paramName || ''}`);
    // this.paramName = decodeURI(paramName);
    // 单个任务跳转 => otherArgs 设置 issueId，将任务设定为展开模式
    if (paramIssueId) {
      let id = paramOpenIssueId || paramIssueId;
      // 都是数字，说明没加密
      if (/^[0-9]+$/.test(id)) {
        try {
          id = await issueApi.encryptIssueId(id);
        } catch (error) {
          Choerodon.prompt(error.message, 'error');
        }
      }
      issueSearchStore.handleFilterChange('issueIds', [id]);
      IssueStore.setClickedRow({
        selectedIssue: {
          issueId: id,
        },
        expand: true,
      });
      props.open({
        path: 'issue',
        props: {
          issueId: id,
          tab: detailTab === 'comment' ? 'comment' : undefined,
          // store: detailStore,
        },
        events: {
          update: () => {
            refresh();
          },
        },
      });
      query();
    } else {
      query(tableProps.pagination.current);
    }
  });
  const getProjectInfo = () => {
    projectApi.loadInfo().then((res) => {
      IssueStore.setProjectInfo(res);
      initFilter();
    });
  };
  useEffect(() => {
    getProjectInfo();
    return () => {
      IssueStore.setClickedRow({ selectedIssue: {}, expand: false });
      IssueStore.setFilterListVisible(false);
      Modal.close('modal');
    };
  }, []);
  // 退出时若有url筛选 则清空缓存内筛选
  useEffect(() => () => {
    if (hasUrlFilter(filterParams)) {
      localPageCacheStore.remove('issues');
    }
  }, [filterParams, hasUrlFilter]);
  const handleCreateIssue = useCallback((issue) => {
    IssueStore.createQuestion(false);
    IssueStore.setDefaultSummary(undefined);
    IssueStore.setDefaultTypeId(undefined);
    IssueStore.setDefaultSprint(undefined);
    IssueStore.setDefaultAssignee(undefined);
    refresh();
  }, [refresh]);
  const handleRowClick = useCallback((record) => {
    query();
  }, [query]);
  const handleSummaryClick = useCallback((record) => {
    open({
      path: 'issue',
      props: {
        issueId: get(record, 'issueId'),
        // store: detailStore,
      },
      events: {
        update: () => {
          refresh();
        },
      },
    });
  }, [open, refresh]);
  const handleClickFilterManage = () => {
    const editFilterInfo = IssueStore.getEditFilterInfo;
    const filterListVisible = IssueStore.getFilterListVisible;
    IssueStore.setFilterListVisible(!filterListVisible);
    IssueStore.setEditFilterInfo(map(editFilterInfo, (item) => Object.assign(item, {
      isEditing:
        false,
    })));
  };
  const handleClear = useCallback(() => {
    setUrlFilter(null);
    const {
      paramChoose, paramCurrentVersion, paramCurrentSprint, paramId,
      paramType, paramIssueId, paramName, paramOpenIssueId,
    } = params;
    if (paramOpenIssueId || paramIssueId || paramChoose || paramType) {
      history.replace(linkUrl(LINK_URL.workListIssue));
    }
  }, [history, params]);

  const handleClickSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };
  const closeBatchModal = useCallback(() => {
    tableProps.setCheckValues([]);
  }, [tableProps]);
  const handleOpenCreateIssue = usePersistFn(() => {
    openCreateIssue({
      onCreate: handleCreateIssue,
      defaultValues: {
        summary: IssueStore.defaultSummary,
      },
      defaultAssignee: IssueStore.defaultAssignee,
      defaultTypeId: IssueStore.defaultTypeId,
      // chosenSprint={IssueStore.defaultSprint}
    });
  });
  return (
    <Page
      className="c7nagile-issue"
    >
      <Header
        title="所有工作项"
      >
        <HeaderButtons items={[
          {
            name: '创建工作项',
            icon: 'playlist_add',
            handler: handleOpenCreateIssue,
            display: true,
          },
          {
            name: '导入工作项',
            icon: 'archive-o',
            handler: () => handleOpenImport({
              onFinish: refresh, action: 'agile_import_issue',
            }),
            display: true,
          },
          {
            name: '导出工作项',
            icon: 'unarchive-o',
            handler: () => {
              const visibleColumns = (cached?.listLayoutColumns || defaultListLayoutColumns).filter((item) => item.display).map((item) => item.columnCode);
              openExportIssueModal(
                issueSearchStore.getAllFields,
                issueSearchStore.isHasFilter ? [...issueSearchStore.chosenFields.values()].filter(((c) => !['issueIds', 'userId'].includes(c.code))) : [],
                tableFields || [],
                visibleColumns,
                tableListMode,
                'agile_export_issue',
              );
            },
            display: true,
          },
          {
            name: '个人筛选',
            icon: 'settings-o',
            handler: handleClickFilterManage,
            display: true,
          },
          {
            display: true,
            element: <CollapseAll
              expandAll={tableProps.expandAll}
              isExpandAll={tableProps.isExpandAll}
              expandAbleKeys={tableProps.expandAbleKeys}
            />,
          },
          {
            display: true,
            icon: 'refresh',
            // funcType: 'flat',
            handler: refresh,
          },
          {
            display: true,
            element: <TableModeSwitch
              data={tableListMode ? 'list' : 'tree'}
              onChange={(mode) => {
                changeTableListMode(mode === 'list');
              }}
              style={{ marginRight: 8 }}
            />,
          },

        ]}
        />
        {/* <div style={{ flex: 1, visibility: 'hidden' }} />
        <TableModeSwitch
          data={tableListMode ? 'list' : 'tree'}
          onChange={(mode) => {
            changeTableListMode(mode === 'list');
          }}
        /> */}
      </Header>
      <Breadcrumb />
      <Content style={theme === 'theme4' ? {} : { paddingTop: 0 }} className="c7nagile-issue-content">
        <IssueSearch
          store={issueSearchStore}
          urlFilter={urlFilter}
          onClear={handleClear}
          onChange={async () => {
            await query();
            // 有筛选，自动展开
            if (issueSearchStore.isHasFilter) {
              tableProps.expandAll(true);
            }
          }}
          onClickSaveFilter={handleClickSaveFilter}
        />
        <IssueTable
          isTree={!tableListMode}
          tableProps={tableProps}
          fields={tableFields}
          listLayoutColumns={cached?.listLayoutColumns ?? defaultListLayoutColumns}
          onCreateIssue={handleCreateIssue}
          onRowClick={handleRowClick}
          typeIdChange={IssueStore.setDefaultTypeId}
          summaryChange={IssueStore.setDefaultSummary}
          assigneeChange={IssueStore.setDefaultAssignee}
          setDefaultSprint={IssueStore.setDefaultSprint}
          IssueStore={IssueStore}
          onSummaryClick={handleSummaryClick}
          onOpenCreateIssue={handleOpenCreateIssue}
        />
        <FilterManage
          visible={IssueStore.filterListVisible}
          setVisible={IssueStore.setFilterListVisible}
          issueSearchStore={issueSearchStore}
        />
        {tableProps.checkValues.length > 0 && (
          <BatchModal
            issueSearchStore={issueSearchStore}
            fields={issueSearchStore.fields}
            selected={tableProps.checkValues}
            onClickEdit={() => {
              issueSearchStore.setBatchAction('edit');
            }}
            close={() => {
              closeBatchModal();
            }}
            onClickDelete={() => {
              issueSearchStore.setBatchAction('delete');
              openBatchDeleteModal({
                selected: tableProps.checkValues,
                close: closeBatchModal,
                onDelete: () => refresh(true),
              });
            }}
            onCancel={() => {
              closeBatchModal();
            }}
            onEdit={() => {
              closeBatchModal();
              refresh();
            }}
            hasBatchDeletePermission={hasBatchDeletePermission}
          />
        )}
        <DetailContainer {...props} />
      </Content>
      <StatusLinkageWSHandle />
    </Page>
  );
});

export default (props) => {
  const { data, isLoading } = useDefaultMyFilter();
  function render() {
    if (isLoading) {
      return <Loading loading />;
    }
    return (
      <StoreProvider {...props} defaultMyFilter={data}>
        <TableCache>
          {(cacheProps) => <Issue {...cacheProps} />}
        </TableCache>
      </StoreProvider>
    );
  }
  return (
    <LoadingProvider style={{ height: '100%', zIndex: 'auto' }}>
      {render()}
    </LoadingProvider>
  );
};
