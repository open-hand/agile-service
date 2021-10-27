import React, { useCallback } from 'react';
import map from 'lodash/map';
import { Button, Form, Select, DateTimePicker, TextField } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { useRemindSettingStore } from '@/components/create-issue/components/remind-setting/stores';

const RemindSettingContent = () => {
  const { formDs } = useRemindSettingStore();

  const handleAdd = useCallback(() => {
    formDs.create();
  }, [formDs]);

  const handleDelete = useCallback((record: Record) => {
    formDs.remove(record);
  }, [formDs]);

  return (
    <div>
      {map(formDs.records, (record: Record) => (
        <Form record={record} columns={6}>
          <Select
            name="timeType"
            colSpan={['at_start', 'at_end'].includes(record.get('timeType')) ? 3 : 1}
            clearButton={false}
          />
          {record.get('timeType') === 'custom_time' && (
            <DateTimePicker name="customTime" colSpan={2} />
          )}
          {!['at_start', 'at_end', 'custom_time'].includes(record.get('timeType')) && ([
            <TextField name="relativeTime" />,
            <Select name="relativeTimeUnit" clearButton={false} />,
          ])}
        </Form>
      ))}
      <Button
        icon="add_playlist"
        onClick={handleAdd}
      >
        添加提醒
      </Button>
    </div>
  );
};

export default RemindSettingContent;
