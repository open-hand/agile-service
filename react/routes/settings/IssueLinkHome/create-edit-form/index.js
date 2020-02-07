import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, TextField,
} from 'choerodon-ui/pro';
  
  
function CreateEditForm(props) {
  const {
    record,
  } = props;

  return (
    <Form
      record={record}
    >
      <TextField
        name="linkName"
      />
      <TextField
        name="outWard"
      />
      <TextField
        name="inWard"
      />
    </Form>
  );
}
  
export default observer(CreateEditForm);
