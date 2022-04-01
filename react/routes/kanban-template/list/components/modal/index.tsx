import React, { useEffect, useCallback } from 'react';
import {
  Modal, Form, DataSet, TextField,
} from 'choerodon-ui/pro';
import { ValueChangeAction } from 'choerodon-ui/pro/lib/text-field/interface';
import { useCreation, usePersistFn } from 'ahooks';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { MAX_LENGTH_KANBAN_NAME } from '@/constants/MAX_LENGTH';
import { kanbanTemplateApiConfig, IKanbanTemplateEdit, kanbanTemplateApi } from '@/api';

type KanbanTemplateModalProps = {
  modal?: IModalProps,
} & (KanbanTemplateCreateModalProps | KanbanTemplateEditModalProps)
interface KanbanTemplateCreateModalProps {
  mode: 'create'
  onSubmit: () => void
}
interface KanbanTemplateEditModalProps {
  mode: 'edit'
  data: IKanbanTemplateEdit
  onSubmit: () => void
}
const isEdit = (props: KanbanTemplateModalProps): props is KanbanTemplateEditModalProps => props.mode === 'edit';
const KanbanTemplateModal: React.FC<KanbanTemplateModalProps> = (props) => {
  const { modal, mode, onSubmit } = props;
  const nameValidator = usePersistFn(async (value) => {
    if (isEdit(props)) {
      if (props.data.name === value) {
        return true;
      }
    }
    const hasSame = await kanbanTemplateApi.checkName(value);
    return hasSame ? '已经有同名模板' : true;
  });
  const dataSet = useCreation(() => new DataSet({
    autoCreate: false,
    transport: {
      update: ({ data }) => props.mode === 'edit' && kanbanTemplateApiConfig.edit(props.data.boardId, {
        boardId: props.data.boardId,
        name: data[0].name,
        objectVersionNumber: props.data.objectVersionNumber,
      }),
      create: ({ data }) => kanbanTemplateApiConfig.create(data[0]),
    },
    fields: [{
      name: 'name',
      label: '看板名称',
      required: true,
      validator: nameValidator,
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    const submitRes = await dataSet.submit();
    if (submitRes === undefined) {
      return true;
    }
    if (submitRes) {
      onSubmit();
      return true;
    }
    return false;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  useEffect(() => {
    if (isEdit(props)) {
      dataSet.loadData([props.data]);
    } else {
      dataSet.create();
    }
  }, [dataSet, mode, props]);
  return (
    <Form dataSet={dataSet}>
      <TextField name="name" maxLength={MAX_LENGTH_KANBAN_NAME} valueChangeAction={'input' as ValueChangeAction} />
    </Form>
  );
};

const openKanbanTemplateModal = (props: KanbanTemplateModalProps) => {
  Modal.open({
    key: 'KanbanTemplateModal',
    title: props.mode === 'create' ? '创建看板模板' : '修改看板模板',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <KanbanTemplateModal {...props} />,
  });
};
export default openKanbanTemplateModal;
