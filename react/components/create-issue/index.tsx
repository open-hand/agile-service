import React, { useEffect, useRef, useState } from 'react';
import { Modal, Form } from 'choerodon-ui/pro';
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
  applyType?: 'agile' | 'program'
  request?: (data: any) => Promise<any>
  showSelectProject?: boolean,
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
    // @ts-ignore
    modal?.handleOk(() => {
      if (showSelectProject && !projectId) {
        // @ts-ignore
        selectProjectRef?.current?.validate();
      }
      return false;
    });
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
    projectId, applyType = 'agile', onCreate, request,
  } = props;
  const handleSubmit: CreateIssueBaseProps['onSubmit'] = async ({ data, fieldList, fileList }) => {
    const res = await (request ?? issueApi.create)(data as any, applyType);
    await fieldApi.createFieldValue(res.issueId, 'agile_issue', fieldList, data.projectId);
    if (fileList && fileList.length > 0) {
      if (fileList.some((one) => !one.url)) {
        await uploadAttachment(fileList, res.issueId, data.projectId);
      }
    }
    onCreate(res);
  };
  const defaultIssueTypeId = isEmpty(props.defaultTypeId) ? localCacheStore.getItem('agile.issue.type.common.selected') : props.defaultTypeId;

  Modal.open({
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    bodyStyle: {
      overflowX: 'hidden',
    },
    key: 'create-issue',
    title: '创建工作项',
    okText: '创建',
    children: <CreateContent onSubmit={handleSubmit} {...omit(props, 'onSubmit')} defaultTypeId={defaultIssueTypeId} />,
  });
};
export default openModal;
