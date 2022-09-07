/* eslint-disable react/require-default-props */

import React, {useCallback, useEffect, useRef, useState,} from 'react';
import {Choerodon} from '@choerodon/boot';
import {Dropdown, Icon, Menu,} from 'choerodon-ui';
import {Button, Spin, TextField} from 'choerodon-ui/pro';
import {ButtonColor, FuncType} from 'choerodon-ui/pro/lib/button/interface';
import {useClickAway, useLockFn, useUpdateEffect,} from 'ahooks';
import {castArray, find, isBoolean, isString, pick,} from 'lodash';
import useProjectIssueTypes, {ProjectIssueTypesConfig} from '@/hooks/data/useProjectIssueTypes';
import {IIssueType, Issue, User} from '@/common/types';
import {checkCanQuickCreate, getQuickCreateDefaultObj, IQuickCreateDefaultValueParams} from '@/utils/quickCreate';
import {fieldApi, issueApi} from '@/api';
import {fields2Map} from '@/utils/defaultValue';
import localCacheStore from '@/stores/common/LocalCacheStore';
import TypeTag from '../TypeTag';
import UserDropdown, {IUserDropDownProps} from '../UserDropdown';
import useDefaultPriority from '@/hooks/data/useDefaultPriority';
import {WATERFALL_TYPE_CODES} from '@/constants/TYPE_CODE';
import {MAX_LENGTH_SUMMARY} from "@/constants/MAX_LENGTH";

type QuickCreateStatus = 'init' | 'success' | 'failed'
type FilterCacheThumbnailKey = 'agile.issue.type.sub.selected' | 'agile.issue.type.common.selected';
interface QuickCreateSubIssueProps extends Pick<IUserDropDownProps, 'assigneeSelected'> {
  priorityId?: string
  projectId?: string
  applyType?: ProjectIssueTypesConfig['applyType']
  parentIssueId?: string
  sprintId: string
  typeCode?: string | string[]
  defaultValues?: Partial<IQuickCreateDefaultValueParams>
  mountCreate?: boolean
  onCreate?: (issue: Issue) => void
  onUserChange?: IUserDropDownProps['onChange']
  onIssueTypeChange?: (issueType?: IIssueType) => void,
  onAwayClick?: (createFn: () => Promise<any>, currentData: { createStatus: QuickCreateStatus, [key: string]: any }, event: MouseEvent | TouchEvent) => void
  /**
   *什么创建状态触发鼠标离开点击事件
   * @default 'success'
   *
   * 'success': 成功时
   * 'failed': 失败时
   * 'init': 初始化时
   *  true: 总是触发
   *  false: 不去触发
   */
  createStatusTriggerAwayClick?: QuickCreateStatus | boolean
  defaultAssignee?: User | undefined
  cantCreateEvent?: (data: { defaultValues?: { summary?: string, sprint?: string, [propsName: string]: any }, defaultTypeId?: string, defaultAssignee?: User }) => void
  summaryChange?: (summary: string) => void,
  typeIdChange?: (typeId: string) => void,
  setDefaultSprint?: (sprintId: string | undefined) => void,
  assigneeChange?: (assigneeId: string | undefined, assignee: User | undefined) => void
  isCanQuickCreate?: (createData: any) => boolean
  beforeClick?: () => boolean // 点击快速创建前函数，如果返回false,则中断操作
  /** 自定义保存到缓存的key */
  saveFilterToCache?: ((issueType: IIssueType) => FilterCacheThumbnailKey) | FilterCacheThumbnailKey

}
/**
 * 过滤一些dom 鼠标响应
 * @param dom
 * @returns
 */
function filterSelfDom(dom?: HTMLElement) {
  if (!dom) {
    return false;
  }
  // 点击自身
  if (dom.classList?.contains('c7n-subTask-quickCreate')) {
    return true;
  }
  // 过滤issueType下拉框
  if (dom.id === 'quickCreateSubIssue-issueType-overlay' || dom.classList?.contains('c7n-agile-userDropdown-overlay-tooltip') || dom.classList?.contains('c7n-agile-type_tag_popup-tip')) {
    return true;
  }
  // 过滤用户下拉框
  if (dom.id === 'agile-userDropdown-overlay' || dom.id === 'c7n-agile-user-tag' || dom.classList?.contains('c7n-agile-userDropdown-overlay-tooltip')) {
    return true;
  }
  return false;
}
const QuickCreateSubIssue: React.FC<QuickCreateSubIssueProps> = ({
  priorityId, parentIssueId, sprintId, onCreate, defaultAssignee, defaultValues, projectId, cantCreateEvent, isCanQuickCreate, typeCode, summaryChange,
  typeIdChange, setDefaultSprint, assigneeChange, mountCreate, onAwayClick, beforeClick, applyType, saveFilterToCache, createStatusTriggerAwayClick = 'success', onUserChange,
  assigneeSelected, onIssueTypeChange,
}) => {
  const { data: issueTypes, isLoading } = useProjectIssueTypes({
    typeCode: typeCode || 'sub_task', projectId, onlyEnabled: true, applyType,
  });
  const { data: defaultPriority } = useDefaultPriority({ projectId }, { enabled: !priorityId });

  const [summary, setSummary] = useState('');
  const [expand, setExpand] = useState(!!mountCreate);
  const [id, setId] = useState<string | undefined>();
  const [createStatus, setCreateStatus] = useState<'init' | 'success' | 'failed'>('init');
  const [loading, setLoading] = useState(false);
  const currentTemplate = useRef<string>('');
  const currentType = issueTypes?.find((t) => t.id === id);
  const userDropDownRef = useRef(null) as IUserDropDownProps['userDropDownRef'];
  const ref = useRef<any>();
  useEffect(() => {
    userDropDownRef.current?.changeSelect(assigneeSelected);
  }, [assigneeSelected]);
  useEffect(() => {
    onIssueTypeChange && onIssueTypeChange(currentType);
  }, [currentType]);
  useEffect(() => {
    if (issueTypes && issueTypes.length > 0) {
      const typeCodes = castArray(typeCode || 'sub_task');
      let localIssueTypeId = localCacheStore.getItem('agile.issue.type.sub.selected');

      if (!typeCodes.includes('sub_task')) {
        localIssueTypeId = localCacheStore.getItem('agile.issue.type.common.selected');
      }
      const newIssueType = find(issueTypes, { id: localIssueTypeId }) || issueTypes[0];
      setId(newIssueType.id);
    }
  }, [issueTypes, typeCode]);
  const handleMenuClick = useCallback(({ key }) => {
    setId(key);
  }, []);
  const handleCreate = useLockFn(async () => {
    if (beforeClick && !beforeClick()) {
      return false;
    }
    const assigneeId = userDropDownRef?.current?.selectedUser?.id;

    if (currentType && summary && summary.trim()) {
      setLoading(true);
      const currentAssignee = userDropDownRef?.current?.selectedUser;

      const setDefaultValues = () => {
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
          assigneeChange(assigneeId, currentAssignee);
        }
      };
      if (!await checkCanQuickCreate(currentType.id, assigneeId, projectId, defaultValues)) {
        if (!cantCreateEvent) {
          Choerodon.prompt('该工作项类型含有必填选项，请使用弹框创建');
          setLoading(false);
        } else {
          const pickDefaultValueKeys = ['estimatedStartTime', 'estimatedEndTime', 'actualStartTime', 'actualEndTime'];
          Choerodon.prompt('请填写标注的必填字段');
          setDefaultValues();
          setLoading(false);
          setCreateStatus('failed');
          handleCancel();
          cantCreateEvent({
            defaultValues: {
              summary,
              sprint: sprintId,
              priority: priorityId,
              ...pick(defaultValues || {}, pickDefaultValueKeys),
            },
            defaultTypeId: currentType.id,
            defaultAssignee: currentAssignee,
          });
        }
        return false;
      }
      const param = {
        schemeCode: 'agile_issue',
        issueTypeId: currentType.id,
        pageCode: 'agile_issue_create',
      };
      const fields = await fieldApi.getFields(param, projectId);
      const fieldsMap = fields2Map(fields);
      const issue = getQuickCreateDefaultObj({
        ...defaultValues,
        summary,
        projectId,
        priorityId: priorityId || defaultPriority?.id,
        [currentType.typeCode === 'bug' ? 'relateIssueId' : 'parentIssueId']: parentIssueId,
        issueTypeId: currentType.id,
        typeCode: currentType.typeCode,
        sprintId,
        assigneeId,
      }, fieldsMap);
      if (isCanQuickCreate && !await isCanQuickCreate(issue)) {
        setDefaultValues();
        setLoading(false);
        setCreateStatus('failed');
        handleCancel();
        cantCreateEvent && cantCreateEvent({
          defaultValues: {
            summary,
            sprint: sprintId,
            priority: priorityId,
          },
          defaultTypeId: currentType.id,
          defaultAssignee: currentAssignee,
        });
        return false;
      }
      let res: any;
      if (applyType === 'waterfall' && WATERFALL_TYPE_CODES.includes(currentType.typeCode)) {
        res = await issueApi.project(projectId).create(issue, applyType);
      } else {
        res = currentType.typeCode === 'sub_task'
          && (issue.parentIssueId || issue.relateIssueId || issue.relateIssueId) ? await issueApi.project(projectId).createSubtask(issue) : await issueApi.project(projectId).create(issue, WATERFALL_TYPE_CODES.includes(currentType.typeCode) ? 'waterfall' : 'agile');
      }

      await fieldApi.project(projectId).quickCreateDefault(res.issueId, {
        schemeCode: 'agile_issue',
        issueTypeId: currentType.id,
        pageCode: 'agile_issue_create',
      });
      setLoading(false);
      setSummary('');
      handleCancel();
      onCreate && onCreate(res);
      const defaultSaveFilterToCache = () => (currentType.typeCode === 'sub_task' ? 'agile.issue.type.sub.selected' : 'agile.issue.type.common.selected');
      const getSaveFilterToCacheFn = (fn: QuickCreateSubIssueProps['saveFilterToCache']) => (typeof fn === 'function' ? fn : () => fn);
      const cacheKey = saveFilterToCache ? getSaveFilterToCacheFn(saveFilterToCache) : defaultSaveFilterToCache;
      currentType && localCacheStore.setItem(cacheKey(currentType)!, currentType.id);

      setCreateStatus('success');
      return true;
    }
    setCreateStatus('failed');
    return false;
  });
  useEffect(() => {
    if (expand && id) {
      fieldApi.project(projectId).getSummaryDefaultValue(id).then((res) => {
        if (summary === currentTemplate.current) {
          currentTemplate.current = res as string;
          setSummary(res as string);
        }
      });
    }
  }, [expand, id, projectId]);
  const handleCancel = useCallback(() => {
    setExpand(false);
  }, []);
  useClickAway((e) => {
    if (e && ((e as MouseEvent).composedPath() as HTMLElement[]).some(filterSelfDom)) {
      e.stopPropagation();
      e.preventDefault();
      return;
    }
    if (!isLoading && onAwayClick) {
      if (isBoolean(createStatusTriggerAwayClick) && createStatusTriggerAwayClick) {
        onAwayClick(handleCreate, {
          createStatus, currentType, currentTemplate, userDropDownRef,
        }, e);
      } else if (isString(createStatusTriggerAwayClick) && createStatusTriggerAwayClick === createStatus) {
        onAwayClick(handleCreate, {
          createStatus, currentType, currentTemplate, userDropDownRef,
        }, e);
      }
    }
  }, ref);
  useUpdateEffect(() => {
    expand && setCreateStatus('init');
  }, [expand]);
  if (isLoading) {
    return null;
  }

  return issueTypes && (priorityId || defaultPriority) ? (
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
                                  tooltip={false}
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
                <UserDropdown userDropDownRef={userDropDownRef} defaultAssignee={defaultAssignee} key={defaultAssignee?.id} projectId={projectId} onChange={onUserChange} />

                <TextField
                  className="hidden-label"
                  autoFocus
                  autoComplete="on"
                  onEnterDown={handleCreate}
                  onInput={(e) =>{
                    if(e && e.target) {
                      setSummary((e.target as any).value);
                    }
                  }}
                  disabled={loading}
                  value={summary}
                  maxLength={MAX_LENGTH_SUMMARY}
                  showLengthInfo={true}
                  placeholder="请输入工作项概要"
                  style={{width: '100%'}}
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
              if (beforeClick && !beforeClick()) {
                return;
              }
              setExpand(true);
            }}
          >
            {applyType === 'waterfall' ? '快速创建子工作项' : '快速创建子任务'}
          </Button>
        )}
    </div>
  ) : null;
};

export default QuickCreateSubIssue;
