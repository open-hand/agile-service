// import React, { forwardRef } from 'react';
// import { demandApi } from '@/api';
// import SelectUser, { SelectUserProps } from '@/components/select/select-user';

// interface Props extends SelectUserProps {
//   projectId?: number
// }
// const SelectUserOutSide: React.FC<Props> = forwardRef(({ projectId, ...props }, ref) => (
//   <SelectUser
//   // @ts-ignore
//     ref={ref}
//     // @ts-ignore
//     request={({ filter, page }) => {
//       if (projectId) {
//         return demandApi.project(projectId).getOutSideUser(filter, page);
//       }
//       return Promise.resolve({
//         list: [],
//       });
//     }}
//     {...props}
//   />
// ));
// export default SelectUserOutSide;
