// import React from 'react';
// import { observer } from 'mobx-react-lite';
// import { useDetailContext } from '@/components/IssueDetail/context';
// import UserHead from '@/components/UserHead';
// import Field from '../field';

// const Creator: React.FC = () => {
//   const { store } = useDetailContext();
//   const { issue: { createUser } } = store;
//   return (
//     <Field label="创建人">
//       <div style={{ padding: '0 0.05rem 0 0.05rem' }}>
//         {
//         createUser ? (
//           <UserHead
//             user={createUser}
//           />
//         ) : (
//           <div>
//             无
//           </div>
//         )
//       }
//       </div>
//     </Field>
//   );
// };

// export default observer(Creator);
