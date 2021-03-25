import React, { useEffect, useCallback } from 'react';
import {
  Modal, Form, DataSet, TextField, TextArea,
} from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { MAX_LENGTH_KANBAN_NAME, MAX_LENGTH_KANBAN_DESCRIPTION } from '@/constants/MAX_LENGTH';
import { kanbanTemplateApiConfig, IKanbanTemplateEdit } from '@/api';
import { useCreation } from 'ahooks';

type KanbanTemplateModalProps = {
  modal?: IModalProps,
} & (KanbanTemplateCreateModalProps | KanbanTemplateEditModalProps)
interface KanbanTemplateCreateModalProps {
  mode: 'create'
}
interface KanbanTemplateEditModalProps {
  mode: 'edit'
  boardId: string
  data: IKanbanTemplateEdit
}
const isEdit = (props: KanbanTemplateModalProps): props is KanbanTemplateEditModalProps => props.mode === 'edit';
const KanbanTemplateModal: React.FC<KanbanTemplateModalProps> = (props) => {
  const { modal, mode } = props;
  const dataSet = useCreation(() => new DataSet({
    autoCreate: false,
    transport: {
      create: ({ data }) => (isEdit(props)
        ? kanbanTemplateApiConfig.edit(props.boardId, data)
        : kanbanTemplateApiConfig.create(data)),
    },
    fields: [{
      name: 'name',
      label: '看板名称',
      required: true,
    }, {
      name: 'description',
      label: '描述',
    }],
  }), []);
  const handleSubmit = useCallback(async () => dataSet.submit(), [dataSet]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  useEffect(() => {
    if (isEdit(props)) {
      dataSet.create(props.data);
    } else {
      dataSet.create();
    }
  }, [dataSet, mode, props]);
  return (
    <Form dataSet={dataSet}>
      <TextField name="name" maxLength={MAX_LENGTH_KANBAN_NAME} />
      <TextArea name="description" maxLength={MAX_LENGTH_KANBAN_DESCRIPTION} />
    </Form>
  );
};

const openKanbanTemplateModal = (props: KanbanTemplateModalProps) => {
  Modal.open({
    key: 'KanbanTemplateModal',
    title: props.mode === 'create' ? '创建看板模板' : '编辑看板模板',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <KanbanTemplateModal {...props} />,
  });
};
export default openKanbanTemplateModal;
