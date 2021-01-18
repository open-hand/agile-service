import React, { useCallback, useEffect, useState } from 'react';
import { Choerodon } from '@choerodon/boot';
import {
  Button, Input, Form, Icon, Dropdown, Menu,
} from 'choerodon-ui';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { useLockFn } from 'ahooks';
import { IIssueType } from '@/common/types';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import { FormProps } from 'choerodon-ui/lib/form';
import { getProjectId } from '@/utils/common';
import { fieldApi, issueApi } from '@/api';
import { WrappedFormUtils } from 'choerodon-ui/lib/form/Form';
import { fields2Map } from '@/utils/defaultValue';
import TypeTag from '../TypeTag';

const FormItem = Form.Item;

interface QuickCreateSubIssueProps extends FormProps {
  priorityId: string
  parentIssueId: string
  sprintId: string
  onCreate?: () => void
}
const QuickCreateSubIssue: React.FC<QuickCreateSubIssueProps> = ({
  form, priorityId, parentIssueId, sprintId, onCreate,
}) => {
  const { data: issueTypes, isLoading } = useProjectIssueTypes({ typeCode: 'sub_task' });
  const [expand, setExpand] = useState(false);
  const [typeCode, setTypeCode] = useState<string | undefined>();
  const [loading, setLoading] = useState(false);
  const currentType = issueTypes?.find((t) => t.typeCode === typeCode);
  useEffect(() => {
    if (issueTypes && issueTypes.length > 0) {
      setTypeCode(issueTypes[0].typeCode);
    }
  }, [issueTypes]);
  const handleMenuClick = useCallback(({ key }) => {
    setTypeCode(key);
  }, []);
  const handleCreate = useLockFn(async () => {
    (form as WrappedFormUtils).validateFields(async (err, values) => {
      const { summary } = values;
      if (currentType && summary && summary.trim()) {
        if (!err) {
          setLoading(true);
          if (!await checkCanQuickCreate(typeCode)) {
            Choerodon.prompt('该问题类型含有必填选项，请使用弹框创建');
            setLoading(false);
            return;
          }
          const param = {
            schemeCode: 'agile_issue',
            context: currentType.typeCode,
            pageCode: 'agile_issue_create',
          };
          const fields = await fieldApi.getFields(param);
          const fieldsMap = fields2Map(fields);
          const issue = {
            summary,
            priorityId,
            priorityCode: `priority-${priorityId}`,
            projectId: getProjectId(),
            parentIssueId,
            issueTypeId: currentType.id,
            sprintId: sprintId || fieldsMap.get('sprint').defaultValue || 0,
            componentIssueRelVOList: fieldsMap.get('component').defaultValueObjs || [],
            labelIssueRelVOList: fieldsMap.get('label').defaultValueObjs || [],
            fixVersionIssueRel: fieldsMap.get('fixVersion').defaultValue || [],
            assigneeId: fieldsMap.get('assignee').defaultValue,
            estimatedEndTime: fieldsMap.get('estimatedEndTime').defaultValue,
            estimatedStartTime: fieldsMap.get('estimatedStartTime').defaultValue,
          };
          const res = await issueApi.createSubtask(issue);
          fieldApi.quickCreateDefault(res.issueId, param);
          setLoading(false);
          handleCancel();
          onCreate && onCreate();
        }
      }
    });
  });
  const handleCancel = useCallback(() => {
    setExpand(false);
  }, []);
  if (isLoading) {
    return null;
  }

  const { getFieldDecorator } = form as WrappedFormUtils;
  return issueTypes ? (
    <div className="c7n-subTask-quickCreate">
      {expand
        ? (
          <Form style={{ width: '100%' }}>
            <div style={{ display: 'block', width: '100%' }}>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                {issueTypes.length > 1 && (
                  <Dropdown
                    overlay={(
                      <Menu
                        style={{
                          background: '#fff',
                          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px rgba(0, 0, 0, 0.12)',
                          borderRadius: '2px',
                        }}
                        onClick={handleMenuClick}
                      >
                        {
                          issueTypes.map((type) => (
                            <Menu.Item key={type.typeCode}>
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
                    <div style={{ display: 'flex', alignItems: 'center' }}>
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
                <FormItem label="summary" style={{ flex: 1, margin: '0 10px', padding: 0 }}>
                  {getFieldDecorator('summary', {
                    rules: [{ required: true, message: '请输入问题概要！' }],
                  })(
                    <Input
                      className="hidden-label"
                      autoFocus
                      autoComplete="on"
                      onPressEnter={handleCreate}
                      maxLength={44}
                      placeholder="请输入问题概要"
                    />,
                  )}
                </FormItem>
                <Button
                  type="primary"
                  funcType="raised"
                  onClick={handleCreate}
                  style={{ margin: '0 10px' }}
                  loading={loading}
                >
                  确定
                </Button>
                <Button
                  funcType="raised"
                  onClick={handleCancel}
                  disabled={loading}
                >
                  取消
                </Button>
              </div>
            </div>
          </Form>
        ) : (
          <Button
            onClick={() => {
              setExpand(true);
            }}
          >
            <Icon type="playlist_add" />
            快速创建子任务
          </Button>
        )}
    </div>
  ) : null;
};
// @ts-ignore
export default Form.create({})(QuickCreateSubIssue);
