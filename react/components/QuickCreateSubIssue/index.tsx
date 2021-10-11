import React, {
  useCallback, useEffect, useRef, useState,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import {
  Input, Icon, Dropdown, Menu,
} from 'choerodon-ui';
import { Button, Spin } from 'choerodon-ui/pro';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import { useClickAway, useLockFn, useUpdateEffect } from 'ahooks';
import { find } from 'lodash';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { IIssueType, Issue, User } from '@/common/types';
import { checkCanQuickCreate, getQuickCreateDefaultObj } from '@/utils/quickCreate';
import { fieldApi, issueApi } from '@/api';
import { fields2Map } from '@/utils/defaultValue';
import localCacheStore from '@/stores/common/LocalCacheStore';
import TypeTag from '../TypeTag';
import UserDropdown from '../UserDropdown';

interface QuickCreateSubIssueProps {
  priorityId: string
  parentIssueId: string
  sprintId: string
  typeCode?: string | string[]
  mountCreate?: boolean
  onCreate?: (issue: Issue) => void
  onAwayClick?: (createFn: any) => void
  defaultAssignee?: User | undefined
  cantCreateEvent?: () => void
  summaryChange?: (summary: string) => void,
  typeIdChange?: (typeId: string) => void,
  setDefaultSprint?: (sprintId: string | undefined) => void,
  assigneeChange?: (assigneeId: string | undefined, assignee: User | undefined) => void
}
const QuickCreateSubIssue: React.FC<QuickCreateSubIssueProps> = ({
  priorityId, parentIssueId, sprintId, onCreate, defaultAssignee, cantCreateEvent, typeCode, summaryChange, typeIdChange, setDefaultSprint, assigneeChange, mountCreate, onAwayClick,
}) => {
  const { data: issueTypes, isLoading } = useProjectIssueTypes({ typeCode: typeCode || 'sub_task', onlyEnabled: true });
  const [summary, setSummary] = useState('');
  const [expand, setExpand] = useState(!!mountCreate);
  const [id, setId] = useState<string | undefined>();
  const [createStatus, setCreateStatus] = useState<'init' | 'success' | 'failed'>('init');
  const [loading, setLoading] = useState(false);
  const currentTemplate = useRef<string>('');
  const currentType = issueTypes?.find((t) => t.id === id);
  const userDropDownRef = useRef<{ selectedUser: User | undefined }>(null);
  const ref = useRef<any>();

  useEffect(() => {
    if (issueTypes && issueTypes.length > 0) {
      const localIssueTypeId = localCacheStore.getItem('agile.issue.type.sub.selected');
      const newIssueType = find(issueTypes, { id: localIssueTypeId }) || issueTypes[0];
      setId(newIssueType.id);
    }
  }, [issueTypes]);
  const handleMenuClick = useCallback(({ key }) => {
    setId(key);
  }, []);
  const handleCreate = useLockFn(async () => {
    const assigneeId = userDropDownRef?.current?.selectedUser?.id;

    if (currentType && summary && summary.trim()) {
      setLoading(true);
      if (!await checkCanQuickCreate(currentType.id, assigneeId)) {
        if (!cantCreateEvent) {
          Choerodon.prompt('该工作项类型含有必填选项，请使用弹框创建');
          setLoading(false);
        } else {
          Choerodon.prompt('请填写标注的必填字段');
          if (summaryChange) {
            summaryChange(summary);
          }
          if (typeIdChange) {
            typeIdChange(currentType.id);
          }
          if (setDefaultSprint) {
            setDefaultSprint(sprintId);
          }
          if (assigneeChange) {
            assigneeChange(assigneeId, userDropDownRef?.current?.selectedUser);
          }
          setLoading(false);
          setCreateStatus('failed');
          handleCancel();
          cantCreateEvent();
        }
        return false;
      }
      const param = {
        schemeCode: 'agile_issue',
        issueTypeId: currentType.id,
        pageCode: 'agile_issue_create',
      };
      const fields = await fieldApi.getFields(param);
      const fieldsMap = fields2Map(fields);
      const issue = getQuickCreateDefaultObj({
        summary,
        priorityId,
        parentIssueId,
        relateIssueId: currentType.typeCode === 'bug' ? parentIssueId : undefined,
        issueTypeId: currentType.id,
        typeCode: currentType.typeCode,
        sprintId,
        assigneeId,
      }, fieldsMap);

      const res = await issueApi.createSubtask(issue);
      await fieldApi.quickCreateDefault(res.issueId, {
        schemeCode: 'agile_issue',
        issueTypeId: currentType.id,
        pageCode: 'agile_issue_create',
      });
      setLoading(false);
      setSummary('');
      handleCancel();
      onCreate && onCreate(res);
      localCacheStore.setItem('agile.issue.type.sub.selected', currentType.id);
      setCreateStatus('success');
      return true;
    }
    setCreateStatus('failed');
    return false;
  });
  useEffect(() => {
    if (expand && id) {
      fieldApi.getSummaryDefaultValue(id).then((res) => {
        if (summary === currentTemplate.current) {
          currentTemplate.current = res as string;
          setSummary(res as string);
        }
      });
    }
  }, [expand, id]);
  const handleCancel = useCallback(() => {
    setExpand(false);
  }, []);
  useClickAway((e) => {
    if (e && (e as MouseEvent).composedPath().some((dom) => (dom as HTMLElement)?.id === 'quickCreateSubIssue-issueType-overlay' || (dom as HTMLElement)?.id === 'agile-userDropdown-overlay')) {
      return;
    }
    !isLoading && createStatus !== 'failed' && onAwayClick && onAwayClick(handleCreate);
  }, ref);
  useUpdateEffect(() => {
    expand && setCreateStatus('init');
  }, [expand]);
  if (isLoading) {
    return null;
  }

  return issueTypes ? (
    <div className="c7n-subTask-quickCreate" ref={ref}>
      {expand
        ? (
          <div style={{ display: 'block', width: '100%' }}>
            <Spin spinning={loading} size={'small' as any}>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                {issueTypes.length > 1 && (
                  <Dropdown
                    overlay={(
                      <Menu
                        style={{
                          background: '#fff',
                          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
                          borderRadius: '2px',
                        }}
                        id="quickCreateSubIssue-issueType-overlay"
                        onClick={handleMenuClick}
                      >
                        {
                          issueTypes.map((type) => (
                            <Menu.Item key={type.id}>
                              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                                <TypeTag
                                  data={type}
                                  showName
                                />
                              </div>
                            </Menu.Item>
                          ))
                        }
                      </Menu>
                    )}
                    trigger={['click']}
                  >
                    <div style={{ display: 'flex', alignItems: 'center', cursor: 'pointer' }}>
                      <TypeTag
                        data={currentType as IIssueType}
                      />
                      <Icon
                        type="arrow_drop_down"
                        style={{ fontSize: 16 }}
                      />
                    </div>
                  </Dropdown>
                )}
                <UserDropdown userDropDownRef={userDropDownRef} defaultAssignee={defaultAssignee} key={defaultAssignee?.id} />

                <Input
                  className="hidden-label"
                  autoFocus
                  autoComplete="on"
                  onPressEnter={handleCreate}
                  onChange={(e) => {
                    setSummary(e.target.value);
                  }}
                  disabled={loading}
                  value={summary}
                  maxLength={44}
                  placeholder="请输入工作项概要"
                />
                <Button
                  color={'primary' as ButtonColor}
                  onClick={handleCreate}
                  style={{ marginLeft: 10 }}
                  loading={loading}
                  disabled={!summary}
                >
                  确定
                </Button>
                <Button
                  onClick={handleCancel}
                  disabled={loading}
                >
                  取消
                </Button>
              </div>
            </Spin>
          </div>
        ) : (
          <Button
            icon="playlist_add"
            funcType={'flat' as FuncType}
            onClick={() => {
              setExpand(true);
            }}
          >
            快速创建子任务
          </Button>
        )}
    </div>
  ) : null;
};
export default QuickCreateSubIssue;
