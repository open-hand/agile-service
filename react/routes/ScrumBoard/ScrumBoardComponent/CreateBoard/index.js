import React, { useEffect, useMemo } from 'react';
import {
  Form, TextField, DataSet,
} from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import { boardApi } from '@/api';

export default inject('AppState')(({ AppState, modal: { handleOk, close }, onCreate }) => {
  async function nameValidator(value, name, record) {
    const isSame = await boardApi.checkName(value);
    return isSame ? '看板名称已存在' : true;
  }
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'name', type: 'string', label: '看板名称', required: true, validator: nameValidator,
      },
    ],
  }));
  async function submit() {
    const isValidate = await dataSet.validate();
    if (isValidate) {
      const [values] = dataSet.toData();
      const board = await boardApi.create(values.name);
      if (!board.failed) {
        onCreate(board);
        close();
      } else {
        Choerodon.prompt(board.message);
      }
    }
    return false;
  }
  useEffect(() => {
    handleOk(submit);
  }, [handleOk]);

  return (
    <Form dataSet={dataSet}>
      <TextField name="name" required maxLength={30} autoFocus valueChangeAction="input" />
    </Form>
  );
});
