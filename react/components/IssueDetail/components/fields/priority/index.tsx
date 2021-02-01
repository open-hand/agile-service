import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import { useDetailContext } from '@/components/IssueDetail/context';
import SelectPriority from '@/components/select/select-priority';
import Field from '../field';
import fieldStyles from '../custom-fields/index.less';

interface Props {
  readonly?: boolean,
}
const Priority: React.FC<Props> = ({ readonly }) => {
  const { store, outside, organizationId } = useDetailContext();
  const { issue } = store;
  const priorityVO = issue.priorityVO || {};
  const { id, colour: color, name } = priorityVO;
  return (
    <Field label="优先级">
      <TextEditToggle
        className={fieldStyles.select}
        disabled={readonly}
        onSubmit={(value: number) => {
          store.update('priorityId', value);
        }}
        initValue={id}
        submitTrigger={['blur', 'change']}
        editor={() => (
          <SelectPriority />
        )}
      >
        {id ? (
          <div
            className="c7n-level"
            style={{
              backgroundColor: `${color}1F`,
              color,
              borderRadius: '2px',
              padding: '0 8px',
              display: 'inline-block',
            }}
          >
            {name}
          </div>
        ) : (
          <div>
            无
          </div>
        )}
      </TextEditToggle>
    </Field>
  );
};

export default observer(Priority);
