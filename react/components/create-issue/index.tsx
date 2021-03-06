import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps, Issue } from '@/common/types';
import { fieldApi, issueApi } from '@/api';
import { uploadAttachment } from '@/utils/richText';
import { omit } from 'lodash';
import BaseComponent, { CreateIssueBaseProps } from './BaseComponent';

export interface CreateIssueProps extends Omit<CreateIssueBaseProps, 'onSubmit'> {
  onCreate: (issue: Issue) => void,
  modal: IModalProps,
  projectId?: string,
  applyType?: 'agile' | 'program'
}

const openModal = (props: CreateIssueProps) => {
  const {
    projectId, applyType = 'agile', onCreate,
  } = props;
  const handleSubmit: CreateIssueBaseProps['onSubmit'] = async ({ data, fieldList, fileList }) => {
    const res = await issueApi.create(data as any, applyType);
    await fieldApi.createFieldValue(res.issueId, 'agile_issue', fieldList);
    if (fileList && fileList.length > 0) {
      if (fileList.some((one) => !one.url)) {
        await uploadAttachment(fileList, res.issueId, projectId);
      }
    }
    onCreate(res);
  };
  Modal.open({
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    key: 'create-issue',
    title: '创建问题',
    okText: '创建',
    children: <BaseComponent onSubmit={handleSubmit} {...omit(props, 'onSubmit')} />,
  });
};
export default openModal;
