import React, { useEffect, useCallback } from 'react';
import {
  Modal, Form, DataSet, TextField, Select,
} from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { MAX_LENGTH_KANBAN_COLUMN_NAME } from '@/constants/MAX_LENGTH';
import { kanbanTemplateApiConfig } from '@/api';
import { useCreation } from 'ahooks';
import StatusTypeTag from '@/components/tag/status-type-tag';

type KanbanTemplateColumnModalProps = {
  modal?: IModalProps,
  boardId: string
  onSubmit: () => void
}

const KanbanTemplateModal: React.FC<KanbanTemplateColumnModalProps> = (props) => {
  const { modal, boardId, onSubmit } = props;
  const dataSet = useCreation(() => new DataSet({
    autoCreate: true,
    transport: {
      create: ({ data }) => {
        const d = data[0];
        return kanbanTemplateApiConfig.createColumn(d.categoryCode, {
          projectId: 0,
          boardId,
          name: d.name,
          categoryCode: d.categoryCode,
        });
      },
    },
    fields: [
      {
        name: 'name',
        label: '列名称',
        required: true,
      },
      {
        name: 'categoryCode',
        label: '类别',
        required: true,
        lookupAxiosConfig: () => ({
          url: '/agile/v1/lookup_values/status_category',
          transformResponse: (data) => ((Array.isArray(data) ? data : JSON.parse(data).lookupValues)).filter((status: any) => status.valueCode !== 'prepare'),
        }),
        textField: 'name',
        valueField: 'valueCode',
      },
    ],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.submit()) {
      onSubmit();
      return true;
    }
    return false;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={dataSet}>
      <TextField name="name" maxLength={MAX_LENGTH_KANBAN_COLUMN_NAME} />
      <Select
        name="categoryCode"
        optionRenderer={({ record }) => (
          <StatusTypeTag
            code={record?.get('valueCode')}
            name={record?.get('name')}
          />
        )}
      />
    </Form>
  );
};

const openKanbanTemplateColumnModal = (props: KanbanTemplateColumnModalProps) => {
  Modal.open({
    key: 'KanbanTemplateColumnModal',
    title: '创建看板列',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <KanbanTemplateModal {...props} />,
  });
};
export default openKanbanTemplateColumnModal;
