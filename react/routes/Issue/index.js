import React, {
  useContext, useRef, useEffect, useState, useCallback, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { useHistory, useLocation } from 'react-router-dom';
import {
  Header, Content, Page, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui';
import queryString from 'querystring';
import { map } from 'lodash';
import CreateIssue from '@/components/CreateIssue';
import { projectApi } from '@/api/Project';
import { issueApi } from '@/api';
import IssueSearch from '@/components/issue-search';
import openSaveFilterModal from '@/components/SaveFilterModal';
import { linkUrl } from '@/utils/to';
import LINK_URL, { getParams } from '@/constants/LINK_URL';
import IssueTable from '@/components/issue-table';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import FilterManage from '@/components/FilterManage';
import DetailContainer, { useDetail } from '@/components/detail-container';
import TableModeSwitch from '@/components/tree-list-switch';
import handleOpenImport from '@/components/ImportIssue/ImportIssue';
import { openExportIssueModal } from './components/ExportIssue';
import IssueStore from '../../stores/project/issue/IssueStore';
import Store, { StoreProvider } from './stores';
import CollapseAll from './components/CollapseAll';
import Modal from './components/Modal';
import './index.less';

const Issue = observer(() => {
  const {
    dataSet, projectId, issueSearchStore, fields, changeTableListMode, tableListMode,
  } = useContext(Store);
  const history = useHistory();
  const location = useLocation();
  const [urlFilter, setUrlFilter] = useState(null);
  const tableRef = useRef();
  const [props] = useDetail();
  IssueStore.setTableRef(tableRef);
  const visibleColumns = useMemo(() => {
    if (localPageCacheStore.getItem('issues.table')) {
      const { columProps } = localPageCacheStore.getItem('issues.table');
      if (Array.isArray(columProps)) {
        return columProps.length > 0 ? columProps.map((item) => item.name) : [];
      }
    }
    return undefined;
  }, []);
  /**
   * 默认此次操作不是删除操作
   * 防止删除此页一条数据时页时停留当前页时出现无数据清空
   * @param {Boolean} isDelete  用于标记是否为删除操作
   */
  const refresh = (isDelete = false) => dataSet.query(
    isDelete
      && dataSet.length === 1
      && dataSet.totalCount > 1
      ? dataSet.currentPage - 1
      : dataSet.currentPage,
  );
  useEffect(() => () => {
    const columProps = tableRef.current
      ? tableRef.current.tableStore.columns.filter((column) => column.name && !column.hidden) : null;
    localPageCacheStore.mergeSetItem('issues.table', { pageInfo: { currentPage: dataSet.currentPage }, columProps });
  }, [dataSet]);
  const hasUrlFilter = useCallback((obj) => {
    const whiteList = ['type', 'category', 'id', 'name', 'organizationId'];
    return Object.keys(obj).some((key) => !whiteList.includes(key));
  }, []);
  const initFilter = async () => {
    const search = queryString.parse(location.search.slice(1));
    const {
      paramChoose, paramCurrentVersion, paramCurrentSprint, paramId,
      paramType, paramIssueId, paramName, paramOpenIssueId, detailTab, ...searchArgs
    } = search;
    if (hasUrlFilter(search)) {
      issueSearchStore.clearAllFilter();
      localPageCacheStore.clear();
    }
    Object.keys(searchArgs).forEach((key) => {
      const value = searchArgs[key];
      switch (key) {
        case 'tableListMode': {
          changeTableListMode(value);
          break;
        }
        case 'statusId':
        case 'sprint':
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
      // 数字，说明没加密
      // eslint-disable-next-line no-restricted-globals
      if (!isNaN(id)) {
        try {
          id = await issueApi.encryptIssueId(id);
        } catch (error) {
          Choerodon.prompt(error.message, 'error');
        }
      }
      issueSearchStore.handleFilterChange('issueIds', [id]);
      // issueSearchStore.handleFilterChange('contents', [`${IssueStore.getProjectInfo.projectCode}-${paramName.split('-')[paramName.split('-').length - 1]}`]);
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
      await IssueStore.query();
    } else {
      const { pageInfo = {} } = localPageCacheStore.getItem('issues.table') || {};

      await IssueStore.query(pageInfo.currentPage);
    }
  };
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

  const handleCreateIssue = (issue) => {
    IssueStore.createQuestion(false);
    dataSet.query();
  };
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
      paramType, paramIssueId, paramName, paramOpenIssueId, ...otherArgs
    } = getParams(location.search);
    if (paramOpenIssueId || paramIssueId || paramChoose || paramType) {
      history.replace(linkUrl(LINK_URL.workListIssue));
    }
    IssueStore.query();
  }, []);

  const handleClickSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };
  return (
    <Page
      className="c7nagile-issue"
      service={[
        'choerodon.code.project.cooperation.work-list.ps.issue',
      ]}
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
              dataSet, tableRef, tableListMode,
              'agile_export_issue',
            );
          }}
        >
          导出问题
        </Button>
        <Button onClick={handleClickFilterManage} icon="settings">个人筛选</Button>
        <CollapseAll dataSet={dataSet} tableRef={tableRef} />
        <div style={{ flex: 1, visibility: 'hidden' }} />
        <TableModeSwitch
          data={tableListMode ? 'list' : 'tree'}
          onChange={(mode) => {
            changeTableListMode(mode === 'list');
          }}
        />
      </Header>
      <Breadcrumb />
      <Content style={{ paddingTop: 0 }} className="c7nagile-issue-content">
        <IssueSearch
          store={issueSearchStore}
          urlFilter={urlFilter}
          onClear={handleClear}
          onChange={() => {
            localPageCacheStore.setItem('issues', issueSearchStore.currentFilter);
            IssueStore.query();
          }}
          onClickSaveFilter={handleClickSaveFilter}
        />
        <IssueTable
          dataSet={dataSet}
          fields={fields}
          tableRef={tableRef}
          visibleColumns={visibleColumns}
          onCreateIssue={handleCreateIssue}
          onRowClick={(record) => {
            props.open({
              path: 'issue',
              props: {
                issueId: record.get('issueId'),
                // store: detailStore,
              },
              events: {
                update: () => {
                  refresh();
                },
              },
            });
            // dataSet.select(record);
            // const editFilterInfo = IssueStore.getEditFilterInfo;
            // IssueStore.setClickedRow({
            //   selectedIssue: {
            //     issueId: record.get('issueId'),
            //   },
            //   expand: true,
            // });
            // IssueStore.setFilterListVisible(false);
            // IssueStore.setEditFilterInfo(map(editFilterInfo, (item) => Object.assign(item, { isEditing: false })));
          }}
          selectedIssue={IssueStore.selectedIssue?.issueId}
        />
        <FilterManage
          visible={IssueStore.filterListVisible}
          setVisible={IssueStore.setFilterListVisible}
          issueSearchStore={issueSearchStore}
        />
        {/* <ExportIssue issueSearchStore={issueSearchStore} dataSet={dataSet} tableRef={tableRef} onCreateIssue={handleCreateIssue} /> */}
        {IssueStore.getCreateQuestion && (
          <CreateIssue
            visible={IssueStore.getCreateQuestion}
            onCancel={() => { IssueStore.createQuestion(false); }}
            onOk={handleCreateIssue}
          />
        )}
        <DetailContainer {...props} />
      </Content>
    </Page>
  );
});

export default (props) => (
  <StoreProvider {...props}>
    <Issue />
  </StoreProvider>
);
