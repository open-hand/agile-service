const storeValue = {};
const useStore = (key, store) => {
  if (store) {
    storeValue[key] = store;
  }
  return storeValue[key];
};
export default useStore;
