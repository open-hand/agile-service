import React, { useMemo, Fragment } from 'react';
import {
  Select, DataSet, Form, Button,
} from 'choerodon-ui/pro';
import { getProjectId } from '@/utils/common';
import TextEditToggle from '@/components/TextEditTogglePro';

const { Text, Edit } = TextEditToggle;
function Test() {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    autoQuery: false,
    paging: false,
    fields: [
      {
        name: 'user',
        type: 'number',
        label: '人员',
        valueField: 'id',
        textField: 'realName',
        required: true,
        lookupAxiosConfig: ({ params }) => ({
          url: `/base/v1/projects/${getProjectId()}/users`,
          method: 'get',
          params,
        }),
      },
    ],
  }), []);
  return (
    <Fragment>
      <TextEditToggle>
        <Text>
          text
        </Text>
        <Edit>
          <Form dataSet={dataSet}>
            <Select
              style={{ width: 200 }}
              name="user"
              searchable
              searchMatcher="param"
            />
          </Form>
        </Edit>        
      </TextEditToggle>
      
      <Button onClick={async () => {
        if (await dataSet.validate()) {
          // eslint-disable-next-line no-console
          console.log('submit');
        }
      }}
      >
        提交
      </Button>
    </Fragment>
  );
}
export default Test;
