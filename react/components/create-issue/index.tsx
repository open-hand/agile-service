import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import { fieldApi, issueApi } from '@/api';
import { uploadAttachment } from '@/utils/richText';
import BaseComponent, { CreateIssueBaseProps } from './BaseComponent';

interface CreateIssueProps {
  // onCreate: (demand: Demand) => void,
  modal: IModalProps,
  projectId?: string,
}

const openModal = (props: CreateIssueProps) => {
  const handleSubmit:CreateIssueBaseProps['onSubmit'] = async ({ data, fieldList, fileList }) => {
    const res = await issueApi.create(data as any, 'agile');
    await fieldApi.createFieldValue(res.issueId, 'agile_issue', fieldList);
    if (fileList && fileList.length > 0) {
      if (fileList.some((one) => !one.url)) {
        await uploadAttachment(fileList, res.issueId, props.projectId);
      }
    }
  };
  Modal.open({
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    key: 'create-issue',
    title: '创建问题',
    okText: '创建',
    children: <BaseComponent onSubmit={handleSubmit} />,
  });
};
export default openModal;
