import React, {
  useContext, useEffect, useState, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import { useHistory } from 'react-router-dom';
import {
  Header, Content, Page, Breadcrumb, Choerodon, useTheme,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui';
import { map, set, get } from 'lodash';
import { useUnmount, usePersistFn } from 'ahooks';
import CreateIssue from '@/components/CreateIssue';
import Loading from '@/components/Loading';
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
  const getTableData = useCallback(({ page, sort, size }) => {
    const search = issueSearchStore.getCustomFieldFilters();
    set(search, 'searchArgs.tree', !tableListMode);
    return issueApi.loadIssues(page, size, sort, search);
  }, [issueSearchStore, tableListMode]);
  const tableProps = useTable(getTableData, {
    rowKey: 'issueId',
    isTree: !tableListMode,
    defaultPage: cached?.pagination?.current,
    defaultPageSize: cached?.pagination?.pageSize,
    // defaultVisibleColumns: cached?.visibleColumns ?? defaultVisibleColumns,
    autoQuery: false,
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
      && tableProps.total > 1
      ? tableProps.current - 1
      : tableProps.curren,
  ), [query, tableProps]);
  const hasUrlFilter = useCallback((obj) => {
    const whiteList = ['type', 'category', 'id', 'name', 'organizationId'];
    return Object.keys(obj).some((key) => !whiteList.includes(key));
  }, []);
  const initFilter = usePersistFn(async () => {
    const {
      paramChoose, paramCurrentVersion, paramCurrentSprint, paramId,
      paramType, paramIssueId, paramName, paramOpenIssueId, detailTab, ...searchArgs
    } = params;
    if (hasUrlFilter(params)) {
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
    };
    if (paramType) {
      prefix = prefixs[paramType];
      issueSearchStore.handleFilterChange(paramType, [paramId]);
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
    if (hasUrlFilter(params)) {
      localPageCacheStore.remove('issues');
    }
  }, [hasUrlFilter, params]);
  const handleCreateIssue = useCallback((issue) => {
    IssueStore.createQuestion(false);
    IssueStore.setDefaultSummary(undefined);
    IssueStore.setDefaultTypeId(undefined);
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
    query();
  }, [history, params, query]);

  const handleClickSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };
  const closeBatchModal = useCallback(() => {
    tableProps.handleCheckAllChange(false);
  }, [tableProps]);
  return (
    <Page
      className="c7nagile-issue"
    >
      <Header
        title="问题管理"
      >
        <Button
          className="leftBtn"
          funcType="flat"
          icon="playlist_add"
          onClick={() => {
            IssueStore.createQuestion(true);
          }}
        >
          创建问题
        </Button>
        <Button
          icon="archive"
          funcType="flat"
          onClick={() => handleOpenImport({
            onFinish: refresh, action: 'agile_import_issue',
          })}
        >
          导入问题
        </Button>
        <Button
          className="leftBtn"
          icon="unarchive"
          funcType="flat"
          onClick={() => {
            openExportIssueModal(
              issueSearchStore.getAllFields,
              issueSearchStore.isHasFilter ? [...issueSearchStore.chosenFields.values()].filter(((c) => !['issueIds', 'contents', 'userId'].includes(c.code))) : [],
              tableListMode,
              'agile_export_issue',
            );
          }}
        >
          导出问题
        </Button>
        <Button onClick={handleClickFilterManage} icon="settings">个人筛选</Button>
        <CollapseAll
          expandAll={tableProps.expandAll}
          isExpandAll={tableProps.isExpandAll}
          expandAbleKeys={tableProps.expandAbleKeys}
        />
        <div style={{ flex: 1, visibility: 'hidden' }} />
        <TableModeSwitch
          data={tableListMode ? 'list' : 'tree'}
          onChange={(mode) => {
            changeTableListMode(mode === 'list');
          }}
        />
      </Header>
      <Breadcrumb />
      <Content style={theme === 'theme4' ? undefined : { paddingTop: 0 }} className="c7nagile-issue-content">
        <IssueSearch
          store={issueSearchStore}
          urlFilter={urlFilter}
          onClear={handleClear}
          onChange={() => {
            localPageCacheStore.setItem('issues', issueSearchStore.currentFilter);
            query();
          }}
          onClickSaveFilter={handleClickSaveFilter}
        />
        <IssueTable
          tableProps={tableProps}
          fields={tableFields}
          listLayoutColumns={cached?.listLayoutColumns ?? defaultListLayoutColumns}
          onCreateIssue={handleCreateIssue}
          onRowClick={handleRowClick}
          typeIdChange={IssueStore.setDefaultTypeId}
          summaryChange={IssueStore.setDefaultSummary}
          IssueStore={IssueStore}
          onSummaryClick={handleSummaryClick}
        />
        <FilterManage
          visible={IssueStore.filterListVisible}
          setVisible={IssueStore.setFilterListVisible}
          issueSearchStore={issueSearchStore}
        />
        {IssueStore.getCreateQuestion && (
          <CreateIssue
            visible={IssueStore.getCreateQuestion}
            onCancel={() => {
              IssueStore.createQuestion(false);
              IssueStore.setDefaultSummary(undefined);
              IssueStore.setDefaultTypeId(undefined);
            }}
            onOk={handleCreateIssue}
            defaultTypeId={IssueStore.defaultTypeId}
            defaultSummary={IssueStore.defaultSummary}
          />
        )}
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
    </Page>
  );
});

export default (props) => {
  const { data, isLoading } = useDefaultMyFilter();
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
};
