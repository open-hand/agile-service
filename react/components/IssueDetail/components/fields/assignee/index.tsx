import React from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectUser from '@/components/select/select-user';
import { User } from '@/common/types';
import { useDetailContext } from '@/components/IssueDetail/context';
import UserTag from '@/components/tag/user-tag';
import Field from '../field';
import fieldStyles from '../custom-fields/index.less';

const Assignee: React.FC = () => {
  const { store } = useDetailContext();
  const { issue } = store;
  const {
    assigneeId, assigneeImageUrl,
    assigneeLoginName, assigneeName, assigneeRealName,
  } = issue;
  const required = false;
  return (
    <Field label="经办人">
      <TextEditToggle
        className={fieldStyles.select}
        onSubmit={(value: User[] | null) => {
          store.update('assignees', value);
        }}
        initValue={(assigneeId && assigneeId.toString()) || undefined}
        editor={({ submit }) => (
          <SelectUser
            clearButton={!required}
            required={required}
            onChange={submit}
            // @ts-ignore
            selectedUser={assigneeId ? {
              id: assigneeId,
              loginName: assigneeLoginName,
              realName: assigneeRealName,
              imageUrl: assigneeImageUrl,
              name: assigneeName,
            } : undefined}
          />
        )}
        disabled
      >
        {
          assigneeId ? (
            <UserTag
              data={{
                // id: assigneeId,
                loginName: assigneeLoginName,
                realName: assigneeRealName,
                imageUrl: assigneeImageUrl,
                tooltip: assigneeName,
              }}
            />
          ) : (
            <div>
              无
            </div>
          )
        }
      </TextEditToggle>
    </Field>
  );
};

export default observer(Assignee);
