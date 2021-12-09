import React, { useEffect, useRef, useState } from 'react';
import { Modal, Form } from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';
import { isEmpty, omit } from 'lodash';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps, Issue } from '@/common/types';
import { fieldApi, issueApi } from '@/api';
import { uploadAttachment } from '@/utils/richText';
import localCacheStore from '@/stores/common/LocalCacheStore';
import BaseComponent, { CreateIssueBaseProps } from './BaseComponent';
import SelectProject from '@/components/select/select-project';

export interface CreateIssueProps extends Omit<CreateIssueBaseProps, 'onSubmit'> {
  onCreate: (issue: Issue) => void,
  /** 创建来源 */
  originFrom?: 'scrumBoard' | 'Backlog'
  hiddenTypeCodes?: string[],
  applyType?: 'agile' | 'program'
  request?: (data: any, applyType?: 'agile' | 'program') => Promise<any>
  showSelectProject?: boolean,
  onCancel?: () => void,
}

const CreateContent = (props: CreateIssueBaseProps) => {
  const {
    showSelectProject = false, projectId, modal,
  } = props;
  const selectProjectRef = useRef();
  const [currentProjectId, setCurrentProjectId] = useState<string | never>();
  const handleProjectChange = (value: string) => {
    setCurrentProjectId(value);
  };
  useEffect(() => {
    if (showSelectProject) {
      // @ts-ignore
      modal?.handleOk(() => {
        if (!projectId) {
          // @ts-ignore
          selectProjectRef?.current?.validate();
        }
        return false;
      });
    }
  }, []);

  return (
    <>
      {showSelectProject && (
        <Form>
          <SelectProject
            // @ts-ignore
            ref={selectProjectRef}
            onChange={handleProjectChange}
            label="所属项目"
            required
            clearButton={false}
            category="N_AGILE"
          />
        </Form>
      )}
      {(!showSelectProject || currentProjectId) && (
        <BaseComponent {...props} projectId={showSelectProject ? currentProjectId : projectId} />
      )}
    </>
  );
};

const openModal = (props: CreateIssueProps) => {
  const {
    projectId, applyType = 'agile', onCreate, onCancel, request, originFrom,
  } = props;
  const handleSubmit: CreateIssueBaseProps['onSubmit'] = async ({ data, fieldList, fileList }) => {
    const res = request ? await request(data as any, applyType) : await issueApi.create(data as any, applyType);
    await fieldApi.createFieldValue(res.issueId, 'agile_issue', fieldList, data.projectId);
    if (fileList && fileList.length > 0) {
      if (fileList.some((one) => !one.url)) {
        await uploadAttachment(fileList, res.issueId, data.projectId);
      }
    }
    onCreate(res);
  };
  const defaultIssueTypeId = isEmpty(props.defaultTypeId) ? localCacheStore.getItem('agile.issue.type.common.selected') : props.defaultTypeId;
  let issueTypeCode: string | string[] | undefined;
  if (originFrom && ['scrumBoard', 'Backlog'].includes(originFrom)) {
    issueTypeCode = ['bug', 'task', 'story', 'sub_task'];
  }
  Modal.open({
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    bodyStyle: {
      overflowX: 'hidden',
    },
    key: 'create-issue',
    title: <C7NFormat
      intlPrefix="agile.common"
      id="create.issue"
    />,
    okText: <C7NFormat
      intlPrefix="boot"
      id="create"
    />,
    onCancel: () => {
      if (onCancel) {
        onCancel();
      }
    },
    children: <CreateContent typeCode={issueTypeCode} onSubmit={handleSubmit} {...omit(props, 'onSubmit')} defaultTypeId={defaultIssueTypeId} />,
  });
};
export default openModal;
