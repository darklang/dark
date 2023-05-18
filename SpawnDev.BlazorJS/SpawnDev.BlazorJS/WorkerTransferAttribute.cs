namespace SpawnDev.BlazorJS {
    [AttributeUsage(AttributeTargets.Property | AttributeTargets.ReturnValue | AttributeTargets.Parameter, Inherited = false, AllowMultiple = true)]
    public class WorkerTransferAttribute : Attribute {
        public bool Transfer { get; private set; } = true;
        public WorkerTransferAttribute() { }
        public WorkerTransferAttribute(bool transfer) => Transfer = transfer;
    }
}
