import React, {
  forwardRef, useCallback, useMemo,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { castArray, omit, uniqBy } from 'lodash';
import useIsProjectMember from '@/hooks/useIsProjectMember';
import { User } from '@/common/types';
import SelectUser, { SelectUserProps } from '@/components/select/select-user';

export interface SelectUserWithAssigneeMeProps extends SelectUserProps {
  onAssigneeMe?: (userInfo: User) => void
}

const SelectUserWithAssigneeMe: React.FC<SelectUserWithAssigneeMeProps> = forwardRef((props, ref: React.Ref<Select>) => {
  const {
    extraOptions: propsExtraOptions, projectId, onAssigneeMe, style,
  } = props;
  const { isProjectMember, userInfo } = useIsProjectMember(projectId);
  const handleClick = useCallback(() => {
    if (userInfo) {
      onAssigneeMe && onAssigneeMe(userInfo);
    }
  }, [onAssigneeMe, userInfo]);

  const extraOptions = useMemo(() => (userInfo
    ? uniqBy([userInfo, ...castArray(propsExtraOptions ?? [])], 'id')
    : propsExtraOptions), [propsExtraOptions, userInfo]);
  return (
    <div style={{ ...style, display: 'flex', alignItems: 'center' }}>
      <SelectUser
        // @ts-ignore
        ref={ref}
        {...omit(props, 'style')}
        extraOptions={extraOptions}
        style={{
          flex: 1,
        }}
      />
      {isProjectMember ? (
        <span
          role="none"
          onClick={handleClick}
          style={{
            cursor: 'pointer',
            marginLeft: '10px',
            color: 'var(--primary-color)',
          }}
        >
          分配给我
        </span>
      ) : null}
    </div>
  );
});
SelectUserWithAssigneeMe.displayName = 'SelectUserWithAssigneeMe';
export default SelectUserWithAssigneeMe;
